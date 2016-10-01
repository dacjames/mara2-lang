package io.dac.mara.core

/**
  * Created by dcollins on 8/21/16.
  */
sealed trait MaraValue


object MaraValue {

  case class IntValue(value: Int) extends MaraValue
  case class StringValue(value: String) extends MaraValue
  case class BoolValue(value: Boolean) extends MaraValue
  case class RecordValue(value: Record[_]) extends MaraValue
  case class ValueParamValue(value: String, typex: MaraType) extends MaraValue
  case class TypeParamValue(value: String, typex: MaraType) extends MaraValue
  case class FunctionValue[E <: Expr](name: String, typeparams: Seq[TypeParamValue], valparams: Seq[ValueParamValue], body: Seq[E]) extends MaraValue
  case class ErrorValue(msg: String) extends MaraValue
  case class UnitValue() extends MaraValue

  object implicits {
    implicit def int2value(x: Int): IntValue = IntValue(x)
    implicit def string2value(x: String): StringValue = StringValue(x)
    implicit def bool2value(x: Boolean): BoolValue = BoolValue(x)

    object truthy {
      implicit def value2bool(value: MaraValue): Boolean = value match {
        case IntValue(x) => IntBoolConverters.int2bool(x)
        case StringValue(_) => false
        case BoolValue(b) => b
      }
    }
  }
}
