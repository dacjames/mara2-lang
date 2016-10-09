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
  }

  case class FunctionType(input: MaraType, output: MaraType) extends MaraType {
    val name = None
  }
}