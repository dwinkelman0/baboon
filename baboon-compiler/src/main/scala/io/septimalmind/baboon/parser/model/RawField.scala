package io.septimalmind.baboon.parser.model

case class RawField(
  name: RawFieldName,
  tpe: RawTypeRef,
  prevName: Option[RawFieldName],
  default: Option[RawDefaultValue] = None,
  normalize: Option[RawFn] = None,
  migrate: Option[RawMigrate] = None,
  /** First-class fn initializer, populated by the parser for both
    * `= fn(...)` property initializers (see `RawDtoMember.FnProperty.fn`)
    * and `@default(fn(...))` annotations (with `default = None` then).
    * Plain `FieldDef`s without a fn initializer carry `None` here.
    * Roles stay on the member wrapper: `FieldDef` + fn => `Default`,
    * `FnProperty` => `Computed`.
    */
  fn: Option[RawFn] = None,
)
