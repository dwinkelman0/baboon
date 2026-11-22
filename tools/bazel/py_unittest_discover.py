"""Bazel test runner: `python -m unittest discover` over dotted packages.

mdl runs `python3 -m unittest discover -s <dir>` with cwd=py-stub. Under Bazel
the test sources (hand-written) and generated code live in different runfiles
trees whose absolute locations vary by config, so directory args are fragile.
Instead each lane passes dotted package names; the runner resolves each
package's __path__ at runtime and discovers tests there. Results are
equivalent (same unittest discovery, same TestCases).
"""

import importlib
import sys
import unittest


def main() -> int:
    loader = unittest.defaultTestLoader
    suite = unittest.TestSuite()
    for dotted in sys.argv[1:]:
        mod = importlib.import_module(dotted)
        paths = list(getattr(mod, "__path__", []))
        if not paths:
            print("error: %s is not a package (no __path__)" % dotted, file=sys.stderr)
            return 2
        for path in paths:
            suite.addTests(loader.discover(path))
    result = unittest.TextTestRunner(verbosity=1).run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
