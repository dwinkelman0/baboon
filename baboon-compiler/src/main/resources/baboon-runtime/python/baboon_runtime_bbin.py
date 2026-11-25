"""Baboon `.bbin` Python runtime (attempt 1): set/provenance/seal helpers.

Each helper maps 1:1 onto a backend-neutral IR node from
``BbinIr.scala`` (phase 6, see ``plans/inputs-ir.md``); the Python emitter
(``BbinPythonLower``) calls into this module instead of inlining semantics:

- ``IndexedSet`` <-> ``IRSet`` (named/compound ``Idx``; anonymous ``{T}``
  sets lower to plain ``list``). Dict-backed, carries the ``idx`` tag plus
  ``holes`` left by filtering (nothing is ever reindexed).
- ``leaf`` <-> ``IRLeaf`` / the direct ``leaf(v, {...})`` constructor.
- ``ProvenanceStack`` / ``with_provenance`` <-> ``IRWithProv``
  (ambient ``with provenance({...})`` blocks, inner-wins merge).
- ``seal_row`` / ``BbinSealError`` <-> ``IRSeal`` + the ``BbinSealSite``
  runtime checks (dynamic completeness, ``lst`` contiguity, provenance).
- ``bbin_filter`` <-> ``IROp.Filter`` (same-set narrowing, holes kept).
- ``bbin_assign_broadcast`` <-> ``IRAssign`` (broadcast/per-row write).

Pure Python, standard library only. Loader-backed sets (``IROp.LoaderRef``)
lower to ``csv_loader``/``json_loader`` (``rel:`` resolved against the
`.bbin` directory, one slot-dict per record); set combinators
(``Cross``/``Zip``/``Concat``/``Reindex``/``Join``) have no lowering yet:
the emitter raises ``NotImplementedError`` for those instead of calling
anything here.
"""

from contextlib import contextmanager

__all__ = [
    "BbinSealError",
    "IndexedSet",
    "ProvenanceStack",
    "leaf",
    "with_provenance",
    "seal_row",
    "bbin_filter",
    "bbin_assign_broadcast",
    "csv_loader",
    "json_loader",
]

#: Directory of the `.bbin` file being executed. The generated module sets
#: this (``baboon_runtime_bbin._BBIN_DIR = "<abs source dir>"``) so that
#: ``rel:`` targets resolve against the `.bbin`, not the process cwd.
#: Left as ``None`` when the module did not set it: loaders then resolve
#: against the current working directory.
_BBIN_DIR = None


class BbinSealError(Exception):
    """Collected seal failure for one ``IRSeal`` site.

    Set-wide sealing attempts every element and collects all errors
    (no throw-first), so the payload is the full ``errors`` list; the
    message joins them for readability.
    """

    def __init__(self, errors):
        self.errors = list(errors)
        super().__init__("bbin seal failed:\n" + "\n".join(f"  - {e}" for e in self.errors))


class IndexedSet:
    """Dict-backed ``IRSet`` with an index tag and filter holes.

    ``entries`` maps index values to row dicts (slot maps); ``idx_tag`` is
    the ``IRSet.idx`` name (``None``-equivalent when unused); ``holes`` is
    the set of keys removed by ``bbin_filter`` — iteration skips them and
    nothing is ever reindexed (``plans/inputs.md``: "Leaves holes").
    """

    def __init__(self, entries=None, idx_tag=None, holes=None):
        self._entries = dict(entries) if entries else {}
        self.idx_tag = idx_tag
        self.holes = set(holes) if holes else set()

    def __len__(self):
        return len(self._entries)

    def __iter__(self):
        return iter(self._entries.values())

    def __contains__(self, key):
        return key in self._entries

    def __getitem__(self, key):
        return self._entries[key]

    def keys(self):
        """Live index values (holes already excluded)."""
        return list(self._entries.keys())

    def items(self):
        """``(index, row)`` pairs, holes excluded."""
        return list(self._entries.items())

    def with_row(self, key, row):
        """Return a copy with ``row`` stored under ``key``."""
        entries = dict(self._entries)
        entries[key] = row
        return IndexedSet(entries, idx_tag=self.idx_tag, holes=set(self.holes))

    def __repr__(self):
        return f"IndexedSet(idx={self.idx_tag!r}, rows={self._entries!r}, holes={sorted(self.holes)!r})"


class ProvenanceStack:
    """Ambient-provenance stack backing ``IRWithProv``.

    ``with_provenance`` pushes one frame; ``current()`` merges all frames
    inner-wins (``plans/inputs.md``: nested blocks merge, inner wins).
    Frames affect only leaves created inside them (see ``leaf``) — existing
    slots are never rewritten.
    """

    _stack = []

    @classmethod
    def current(cls):
        """Merged ambient provenance map (inner frames win)."""
        merged = {}
        for frame in cls._stack:
            merged.update(frame)
        return merged

    @classmethod
    def push(cls, fields):
        cls._stack.append(dict(fields))

    @classmethod
    def pop(cls):
        cls._stack.pop()


@contextmanager
def with_provenance(fields):
    """``with with_provenance({...})`` block <-> ``IRWithProv``."""
    ProvenanceStack.push(fields or {})
    try:
        yield ProvenanceStack.current()
    finally:
        ProvenanceStack.pop()


def leaf(value, prov=None):
    """``leaf(value, prov)`` <-> ``IRLeaf`` / direct leaf constructor.

    Attaches provenance to one new leaf: the explicit ``prov`` map wins
    over the ambient ``ProvenanceStack`` frame (more-inner wins).
    """
    merged = ProvenanceStack.current()
    if prov:
        merged.update(dict(prov))
    return {"value": value, "provenance": merged}


def _check_complete(prefix, value, errors):
    """Collect dynamic-completeness errors: ``None`` marks an unfilled slot."""
    if value is None:
        errors.append(f"incomplete slot {prefix!r}: no value filled")
    elif isinstance(value, dict):
        for k, v in value.items():
            _check_complete(f"{prefix}.{k}", v, errors)
    elif isinstance(value, list):
        for i, v in enumerate(value):
            _check_complete(f"{prefix}[{i}]", v, errors)


def _check_contiguity(prefix, value, errors):
    """Collect ``lst``-gap errors: ``None`` holes inside list values."""
    if isinstance(value, dict):
        for k, v in value.items():
            _check_contiguity(f"{prefix}.{k}", v, errors)
    elif isinstance(value, list):
        for i, v in enumerate(value):
            if v is None:
                errors.append(f"lst gap at {prefix}[{i}]: hole in list slot")
            else:
                _check_contiguity(f"{prefix}[{i}]", v, errors)


def _check_provenance(prefix, value, errors):
    """Collect provenance errors: a defined part must itself be complete."""
    if isinstance(value, dict):
        prov = value.get("_provenance")
        if isinstance(prov, dict):
            for k, v in prov.items():
                if v is None or v == "":
                    errors.append(f"incomplete provenance at {prefix}: field {k!r} is empty")
        for k, v in value.items():
            if k != "_provenance":
                _check_provenance(f"{prefix}.{k}", v, errors)
    elif isinstance(value, list):
        for i, v in enumerate(value):
            _check_provenance(f"{prefix}[{i}]", v, errors)


def seal_row(partial, constructor=None):
    """``seal_row(partial, constructor)`` <-> ``IRSeal``.

    Runs the ``BbinSealSite`` runtime checks — dynamic completeness
    (every slot filled recursively; empty containers are complete),
    ``lst`` contiguity (no ``None`` gaps), and provenance completeness
    (any defined part triggers subtree validation) — collecting *all*
    errors and raising ``BbinSealError(list)`` when any fail.

    ``constructor`` builds the sealed instance from the validated slot
    map: a callable is invoked with a copy of ``partial``; a ``str`` (the
    sealed type name, as emitted by ``BbinPythonLower``) or ``None`` means
    constructor wiring is deferred and the validated slot map itself is
    returned. The default→migrate→normalize→validate pipeline runs here
    once constructors are wired.
    """
    snapshot = dict(partial)
    errors = []
    _check_complete("<row>", snapshot, errors)
    _check_contiguity("<row>", snapshot, errors)
    _check_provenance("<row>", snapshot, errors)
    if errors:
        raise BbinSealError(errors)
    if callable(constructor):
        return constructor(dict(snapshot))
    return dict(snapshot)


def bbin_filter(set_obj, pred):
    """``bbin_filter(set, pred)`` <-> ``IROp.Filter``.

    Narrows to the entries passing ``pred``; failing entries become
    ``holes`` — nothing is reindexed. ``IndexedSet`` input preserves the
    ``idx`` tag; plain lists return a filtered list.
    """
    if isinstance(set_obj, IndexedSet):
        kept = {}
        holes = set(set_obj.holes)
        for key, row in set_obj.items():
            if pred(row):
                kept[key] = row
            else:
                holes.add(key)
        return IndexedSet(kept, idx_tag=set_obj.idx_tag, holes=holes)
    return [row for row in set_obj if pred(row)]


def _resolve_path(row, segments, create=False):
    """Walk ``segments`` to the parent dict of the addressed slot."""
    current = row
    for seg in segments[:-1]:
        nxt = current.get(seg)
        if not isinstance(nxt, dict):
            if not create:
                raise KeyError(f"bbin assign: no slot {seg!r} in {current!r}")
            nxt = {}
            current[seg] = nxt
        current = nxt
    return current


def bbin_assign_broadcast(target, path, value, op="set"):
    """``bbin_assign_broadcast(t, path, v, op)`` <-> ``IRAssign``.

    Broadcasts one write to every row: ``IndexedSet`` targets apply it
    per row, plain-dict targets apply it once. ``op`` is ``"set"``
    (``=``), ``"add"`` (``+=``) or ``"mul"`` (``*=``); ``path`` is a
    dotted slot path (``"op.voltage"``) or a segment list. Reading an
    unfilled (``None``) slot on a partial is a runtime error.
    """
    segments = path.split(".") if isinstance(path, str) else list(path)
    if not segments:
        raise ValueError("bbin assign: empty path")

    def apply(row):
        if not isinstance(row, dict):
            raise TypeError(f"bbin assign: cannot write slot into {row!r}")
        parent = _resolve_path(row, segments, create=(op == "set"))
        slot = segments[-1]
        if op == "set":
            parent[slot] = value
        elif op == "add":
            current = parent.get(slot)
            if current is None:
                raise RuntimeError(f"bbin assign: reading unfilled slot {slot!r} on a partial")
            parent[slot] = current + value
        elif op == "mul":
            current = parent.get(slot)
            if current is None:
                raise RuntimeError(f"bbin assign: reading unfilled slot {slot!r} on a partial")
            parent[slot] = current * value
        else:
            raise ValueError(f"bbin assign: unknown op {op!r}")
        return row

    if isinstance(target, IndexedSet):
        for row in target:
            apply(row)
        return target
    if isinstance(target, list):
        for row in target:
            apply(row)
        return target
    apply(target)
    return target


def _loader_path(rel, base_dir=None):
    """Resolve a ``rel:`` target against the `.bbin` directory.

    ``base_dir`` is the ``_BBIN_DIR`` baked into the generated module;
    without it the module-global ``_BBIN_DIR`` applies, otherwise the
    process working directory. Absolute ``rel`` passes through unchanged.
    """
    import os

    base = base_dir or _BBIN_DIR or os.getcwd()
    return rel if os.path.isabs(rel) else os.path.join(base, rel)


def _lookup_col(record, colpath):
    """Walk ``colpath`` segments (``["a", "b"]`` or ``["a", 0]``) in a record."""
    current = record
    for seg in colpath:
        if isinstance(current, dict):
            if seg not in current:
                raise KeyError(f"bbin loader: no column {seg!r} in record {record!r}")
            current = current[seg]
        elif isinstance(current, (list, tuple)):
            current = current[seg]
        else:
            raise KeyError(f"bbin loader: cannot step into {current!r} via {seg!r}")
    return current


def _apply_maps(records, row_fn, maps):
    """Build one slot-dict per record: ``row_fn`` wins, else ``maps``.

    ``maps`` is a list of ``(slot, colpath)`` pairs (as emitted by
    ``BbinPythonLower`` from ``.map(slot, .column)``); without either,
    the raw record dict is the row.
    """
    rows = []
    for rec in records:
        if row_fn is not None:
            rows.append(row_fn(rec))
        elif maps:
            rows.append({slot: _lookup_col(rec, col) for slot, col in maps})
        else:
            rows.append(dict(rec))
    return rows


def csv_loader(rel, row_fn=None, maps=None, base_dir=None):
    """``csv_loader(rel, row_fn, maps)`` <-> ``IROp.LoaderRef`` (csv).

    Reads the ``rel:`` target relative to the `.bbin` (see
    ``_loader_path``) with ``csv.DictReader`` and builds one slot-dict
    per data row via ``_apply_maps``. A missing file raises the plain
    ``FileNotFoundError`` naming the resolved path.
    """
    import csv

    path = _loader_path(rel, base_dir)
    with open(path, newline="") as f:
        records = list(csv.DictReader(f))
    return _apply_maps(records, row_fn, maps)


def json_loader(rel, row_fn=None, maps=None, base_dir=None):
    """``json_loader(rel, row_fn, maps)`` <-> ``IROp.LoaderRef`` (json).

    Reads the ``rel:`` target relative to the `.bbin` (see
    ``_loader_path``): a top-level array is the row list, any other JSON
    value is a single row. Slot-dicts come from ``_apply_maps``.
    """
    import json

    path = _loader_path(rel, base_dir)
    with open(path) as f:
        payload = json.load(f)
    records = payload if isinstance(payload, list) else [payload]
    return _apply_maps(records, row_fn, maps)
