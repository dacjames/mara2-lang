package io.dac.mara.core

sealed trait Pair
object Pair {
  sealed trait Type extends Pair
  case class Type1(typex: String) extends Type {
    override def toString: String = typex
  }
  case class Type2(typex: String, qualifier: String) extends Type {
    override def toString: String = s"$typex: $qualifier"
  }
  object Type {
    def unapply(arg: Type): Option[(String, Option[String])] = arg match {
      case Type1(typex) => Some(typex, None)
      case Type2(typex, qualifier) => Some(typex, Some(qualifier))
    }
  }

  sealed trait Value extends Pair
  case class Value1(value: String) extends Value {
    override def toString: String = value
  }
  case class Value2(value: String, qualifier: String) extends Value {
    override def toString: String = s"$value: $qualifier"
  }
  object Value {
    def unapply(arg: Value): Option[(String, Option[String])] = arg match {
      case Value1(value) => Some(value, None)
      case Value2(value, qualifier) => Some(value, Some(qualifier))
    }
  }

  def unapply(arg: Pair): Option[(String, Option[String])] = arg match {
    case arg: Type => Type.unapply(arg)
    case arg: Value => Value.unapply(arg)
  }
}

