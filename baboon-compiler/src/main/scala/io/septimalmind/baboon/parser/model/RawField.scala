package io.septimalmind.baboon.parser.model

case class RawField(
  name: RawFieldName,
  tpe: RawTypeRef,
  prevName: Option[RawFieldName],
  default: Option[RawDefaultValue] = None,
  normalize: Option[RawFn] = None,
  migrate: Option[RawMigrate] = None,
)
