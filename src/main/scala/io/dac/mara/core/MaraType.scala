package io.dac.mara.core

sealed trait MaraType {
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
  case class InferableType() extends MaraType {
    val name = None
  }
  case class ErrorType(msg: String) extends MaraType {
    val name = None
  }
}