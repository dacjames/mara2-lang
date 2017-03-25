package io.dac.mara.core

import com.typesafe.scalalogging.LazyLogging

import scala.collection.{Map, mutable}

object Builtins {
  import MaraType._
  import MaraValue._

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
  import MaraType._
  import MaraValue._

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

  trait ScopeLike[A, B] extends mutable.Map[A, B] with mutable.MapLike[A, B, ScopeLike[A, B]]  {
    protected val parentOpt: Option[ScopeLike[A, B]] = None
    private[this] var space: mutable.Map[A, B] = mutable.Map.empty

    override def +=(kv: (A, B)) = {
      space += kv
      this
    }

    override def -=(key: A): ScopeLike.this.type = {
      space -= key
      this
    }

    override def get(key: A): Option[B] = {
      val inSpace = space.get(key)
      inSpace match {
        case Some(_) => inSpace
        case None => parentOpt match {
          case Some(parent) => parent.get(key)
          case None => None
        }
      }
    }

    override def iterator: Iterator[(A, B)] = parentOpt match {
      case None => space.iterator
      case Some(parent) =>
        new Iterator[(A, B)] {
          private[this] val spaceIter = space.iterator
          private[this] val parentIter = parent.iterator
          override def hasNext: Boolean = spaceIter.hasNext || parentIter.hasNext
          override def next(): (A, B) =
            if (spaceIter.hasNext) { spaceIter.next() }
            else { parentIter.next() }
        }
    }

    override def seq: mutable.Map[A, B] = parentOpt match {
      case None => space.seq
      case Some(parent) => parent.seq ++ space.seq
    }

    override def empty: ScopeLike[A, B] = this
  }


  class Scope[Kind <: MaraRoot](parent: Option[Scope[Kind]]) extends ScopeLike[Name[Kind], Option[Binding[Kind]]] {
    override val parentOpt = parent
    def push: Scope[Kind] = new Scope(Some(this))
    def pop: Option[Scope[Kind]] = parentOpt
  }
  object Scope {
    def empty[Kind <: MaraRoot] = new Scope[Kind](None)
  }

}


/**
  * Created by dcollins on 10/1/16.
  */
trait Namespace extends LazyLogging {
  import NamespaceTypes._

  private[this] var valuespace: Scope[MaraValue] = Scope.empty
  private[this] var typespace: Scope[MaraType] = Scope.empty

  private[this] def space[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]]) = {
    if (ns eq valuespace) { "Value" }
    else if (ns eq typespace) { "Type" }
    else { "Impossible" }
  }


  private[this] def lookupFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A]): A = {
    val result = ns.get(name).map { (bindingOpt) =>
      bindingOpt.map(_.elem).getOrElse(name.undeclared)
    }.getOrElse(name.unassigned)
    logger.trace(s"Lookup ${space(ns)} ${name.value} -> ${result}")
    result
  }


  private[this] def declareFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A]) = {
    ns += (name -> None)
    logger.trace(s"Declared ${space(ns)} ${name.value}")
    ns

  }

  private[this] def bindFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A], binding: Binding[A]): A = {
    ns += (name -> Some(binding))
    logger.trace(s"Bound ${space(ns)} ${name.value} -> ${binding.elem}")
    binding.elem
  }

  private[this] def unbindFromNamespace[A <: MaraRoot](ns: mutable.Map[Name[A], Option[Binding[A]]], name: Name[A]) = {
    ns -= name
    logger.trace(s"Unbound ${space(ns)} ${name.value}")
    ns
  }

  private[this] def pushScope() = {
    valuespace = valuespace.push
    logger.trace(s"Pushed new valuespace")

    typespace = typespace.push
    logger.trace(s"Push new typespace")

    this
  }

  private[this] def popScope() = {
    valuespace = valuespace.pop.get
    logger.trace(s"Popped valuespace")
    typespace = typespace.pop.get
    logger.trace(s"Popped typespace")
    this
  }


  def lookupValue(name: String) = lookupFromNamespace(valuespace, ValueName(name))
  def declareValue(name: String) = declareFromNamespace(valuespace, ValueName(name))
  def bindValue(name: String, value: MaraValue) = bindFromNamespace(valuespace, ValueName(name), ValueBinding(value))
  def unbindValue(name: String) = lookupFromNamespace(valuespace, ValueName(name))

  def lookupType(name: String): MaraType = lookupFromNamespace(typespace, TypeName(name))
  def declareType(name: String) = declareFromNamespace(typespace, TypeName(name))
  def bindType(name: String, typex: MaraType) = bindFromNamespace(typespace, TypeName(name), TypeBinding(typex))
  def unbindType(name: String) = lookupFromNamespace(typespace, TypeName(name))


  def inNewScope[T](f: => T): T = {
    pushScope()
    val result = f
    popScope()
    result
  }

  require {
    Builtins.types.foreach {
      case (name, typex) => bindType(name, typex)
    }
    true
  }

  require {
    Builtins.values.foreach {
      case (name, value) => bindValue(name, value)
    }
    true
  }

}
