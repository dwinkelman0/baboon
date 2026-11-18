package io.septimalmind.baboon.parser.model

sealed trait RawLocatorSeg

object RawLocatorSeg {
  final case class Field(name: String) extends RawLocatorSeg
  final case class Variant(name: String) extends RawLocatorSeg
  final case class Key(name: String) extends RawLocatorSeg // "$var"
}

final case class RawStarCapture(chainName: String, steps: List[RawLocatorSeg])

final case class RawCapture(
  name: String,
  rootType: RawTypeRef,
  star: Option[RawStarCapture],
  locator: List[RawLocatorSeg],
)

final case class RawFnParam(name: String, tpe: RawTypeRef)

final case class RawBody(letBindings: List[(String, BodyExpr)], result: BodyExpr)

final case class RawFn(captures: List[RawCapture], params: List[RawFnParam], body: RawBody)

final case class RawMigrate(fn: RawFn, sources: List[String])
