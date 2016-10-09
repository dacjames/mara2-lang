package io.dac.mara.core

sealed trait MaraType extends MaraRoot {
  def name: Option[String]
}
object MaraType {
  case class AnyType() extends MaraType {
    val name = Some("Any")
  }
  case class BoolType() extends MaraType {
    val name = Some("Bool")
  }
  case class IntType() extends MaraType {
    val name = Some("Int")
  }
  case class StringType() extends MaraType {
    val name = Some("String")
  }
  case class InferrableType() extends MaraType {
    val name = None
  }
  case class ErrorType(msg: String) extends MaraType {
    val name = None
  }
  case class UnitType() extends MaraType {
    val name = Some("Unit")
  }
  case class RecordType(fields: Seq[MaraType]) extends MaraType {
    val name = None

    private[MaraType] def compareWith(other: RecordType)(pred: (MaraType, MaraType) => Boolean): Boolean =
      this.fields.length == other.fields.length &&
        this.fields.zip(other.fields).forall(p => pred(p._1, p._2))

  }
  case class FunctionType(input: MaraType, output: MaraType) extends MaraType {
    val name = None
  }

  def isSubtype(a: MaraType, b: MaraType): Boolean = (a, b) match {
    case (_, AnyType()) => true
    case (_, ErrorType(_)) => false
    case (a: RecordType, b: RecordType) => a.compareWith(b)(isSubtype)
    case (a: FunctionType, b: FunctionType) =>
      isSubtype(a.input, b.input) && isSupertype(a.output, b.output)
    case _ => a == b
  }

  def isSupertype(a: MaraType, b: MaraType): Boolean = (a, b) match {
    case (AnyType(), _) => true
    case (ErrorType(_), _) => false
    case (a: RecordType, b: RecordType) => a.compareWith(b)(isSupertype)
    case _ => a == b
  }
}

