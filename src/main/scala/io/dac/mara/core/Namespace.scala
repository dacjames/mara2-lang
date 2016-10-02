package io.dac.mara.core

import io.dac.mara.core.MaraType.ErrorType
import io.dac.mara.core.MaraValue.ErrorValue
import sun.rmi.rmic.iiop.ValueType

import scala.collection.mutable

object Builtins {
  import io.dac.mara.core.MaraType._
  import io.dac.mara.core.MaraValue._

  lazy val types = Map(
    "Any" -> AnyType(),
    "String" -> StringType(),
    "Int" -> IntType(),
    "Bool" -> BoolType()
  )

  lazy val values = Map(
    "true" -> BoolValue(true),
    "false" -> BoolValue(false)
  )

  require {
    types.forall {
      case (name, typex) => typex.name.contains(name)
    }
  }
}

object NamespaceTypes {
  sealed trait Name[Kind <: MaraRoot] {
    def kind: String
    def value: String
    def unassigned: Kind
    def undeclared: Kind
  }
  case class TypeName(value: String) extends Name[MaraType] {
    val kind: String = "Type"
    override def unassigned: MaraType = ErrorType(s"Unassigned ${kind} ${value}")
    override def undeclared: MaraType = ErrorType(s"Undeclared ${kind} ${value}")
  }
  case class ValueName(value: String) extends Name[MaraValue] {
    val kind: String = "Value"
    override def undeclared: MaraValue = ErrorValue(s"Unassigned ${kind} ${value}")
    override def unassigned: MaraValue = ErrorValue(s"Undeclared ${kind} ${value}")
  }

  sealed trait Binding[Elem] {
    def elem: Elem
  }
  case class TypeBinding(elem: MaraType) extends Binding[MaraType]
  case class ValueBinding(elem: MaraValue) extends Binding[MaraValue]

}


/**
  * Created by dcollins on 10/1/16.
  */
trait Namespace {
  import NamespaceTypes._

  private val valuespace: mutable.Map[Name[MaraValue], Option[Binding[MaraValue]]] = mutable.Map.empty
  private val typespace: mutable.Map[Name[MaraType], Option[Binding[MaraType]]] = mutable.Map.empty


  private[this] def lookupFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A]): A = {
    val result = ns.get(name).map { (bindingOpt) =>
      bindingOpt.map(_.elem).getOrElse(name.undeclared)
    }.getOrElse(name.unassigned)
    result
  }


  private[this] def declareFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A]) = {
    ns += (name -> None)
  }

  private[this] def bindFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A], binding: Binding[A]): A = {
    ns += (name -> Some(binding))
    binding.elem
  }

  private[this] def unbindFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A]) = {
    ns -= name
  }


  def lookupValue(name: String) = lookupFromNamespace(valuespace, ValueName(name))
  def declareValue(name: String) = declareFromNamespace(valuespace, ValueName(name))
  def bindValue(name: String, value: MaraValue) = bindFromNamespace(valuespace, ValueName(name), ValueBinding(value))
  def unbindValue(name: String) = lookupFromNamespace(valuespace, ValueName(name))


  def lookupType(name: String): MaraType = lookupFromNamespace(typespace, TypeName(name))
  def declareType(name: String) = declareFromNamespace(typespace, TypeName(name))
  def bindType(name: String, typex: MaraType) = bindFromNamespace(typespace, TypeName(name), TypeBinding(typex))
  def unbindType(name: String) = lookupFromNamespace(typespace, TypeName(name))


  require {
    Builtins.types.foreach {
      case (name, typex) => bindType(name, typex)
    }

    Builtins.values.foreach {
      case (name, value) => bindValue(name, value)
    }
    true
  }


}
