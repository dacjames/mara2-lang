package io.dac.mara.core

import com.typesafe.scalalogging.LazyLogging

/**
  * Created by dcollins on 10/1/16.
  */
class Namespace extends LazyLogging {
  import MaraType._
  import MaraValue._
  import MaraAttr._


  var valuespace: Scope[MaraValue] = Scope.empty
  var typespace: Scope[MaraType] = Scope.empty
  var attrspace: Scope[MaraAttr] = Scope.empty

  def clear(): Unit = {
    valuespace = Scope.empty
    typespace = Scope.empty
    attrspace = Scope.empty
  }

  abstract class UniqueSpace[A] {
    def space: Scope[A]
    def name: String
    def undeclared(name: String): A
    def unassigned(name: String): A
  }

  implicit object ValueSpace extends UniqueSpace[MaraValue] {
    override def space: Scope[MaraValue] = valuespace
    override def name: String = "Value"
    override def undeclared(name: String): MaraValue =
      ErrorValue(s"Undeclared Value ${name}")
    override def unassigned(name: String): MaraValue =
      ErrorValue(s"Unassigned Value ${name}")
  }

  implicit object TypeSpace extends UniqueSpace[MaraType] {
    override def space: Scope[MaraType] = typespace
    override def name: String = "Type"
    override def undeclared(name: String): MaraType =
      ErrorType(s"Undeclared Type ${name}")
    override def unassigned(name: String): MaraType =
      ErrorType(s"Unassigned Type ${name}")
  }

  implicit object AttributeSpace extends UniqueSpace[MaraAttr] {
    override def space: Scope[MaraAttr] = attrspace
    override def name: String = "Attribute"
    override def undeclared(name: String): MaraAttr =
      ErrorAttr(s"Undeclared Attribute ${name}")
    override def unassigned(name: String): MaraAttr =
      ErrorAttr(s"Unassigned Attribute ${name}")
  }

  private[this] def lookup[A](name: String)(implicit kind: UniqueSpace[A]): A = {
    val ns = kind.space

    val result = ns.get(name).map { (bindingOpt) =>
      bindingOpt.getOrElse(kind.undeclared(name))
    }.getOrElse(kind.unassigned(name))
    logger.trace(s"Lookup ${kind.name} ${name} -> ${result}")
    result
  }


  private[this] def declare[A](name: String)(implicit kind: UniqueSpace[A]): Scope[A] = {
    val ns = kind.space
    ns += (name -> None)
    logger.trace(s"Declared ${kind.name} ${name}")
    ns
  }

  private[this] def bind[A](name: String, binding: A)(implicit kind: UniqueSpace[A]): A = {
    val ns = kind.space
    ns += (name -> Some(binding))
    logger.trace(s"Bound ${kind.name} ${name} -> ${binding}")
    binding
  }

  private[this] def unbind[A](name: String)(implicit kind: UniqueSpace[A]): Scope[A] = {
    val ns = kind.space
    ns -= name
    logger.trace(s"Unbound ${kind.name} ${name}")
    ns
  }

  def pushScope(): Namespace = {
    valuespace = valuespace.push
    logger.trace(s"Pushed new valuespace")

    typespace = typespace.push
    logger.trace(s"Push new typespace")

    this
  }

  def popScope(): Namespace = {
    valuespace = valuespace.pop.get
    logger.trace(s"Popped valuespace")
    typespace = typespace.pop.get
    logger.trace(s"Popped typespace")
    this
  }

  def lookupValue(name: String): MaraValue = lookup[MaraValue](name)
  def declareValue(name: String): Scope[MaraValue] = declare[MaraValue](name)
  def bindValue(name: String, value: MaraValue): MaraValue = bind[MaraValue](name, value)
  def unbindValue(name: String): Scope[MaraValue] = unbind[MaraValue](name)

  def lookupType(name: String): MaraType = lookup[MaraType](name)
  def declareType(name: String): Scope[MaraType] = declare[MaraType](name)
  def bindType(name: String, typex: MaraType): MaraType = bind[MaraType](name, typex)
  def unbindType(name: String): Scope[MaraType] = unbind[MaraType](name)

  private[this] def attrKey(name: String, attr: Int) = s"${name}:${attr}"

  def lookupAttr[A <: MaraAttr: AttrKey](name: String): MaraAttr = lookup[MaraAttr](attrKey(name, implicitly[AttrKey[A]].key))
  def declareAttr[A <: MaraAttr: AttrKey](name: String): Scope[MaraAttr] = declare[MaraAttr](attrKey(name, implicitly[AttrKey[A]].key))
  def bindAttr[A <: MaraAttr: AttrKey](name: String, attr: A): MaraAttr = bind[MaraAttr](attrKey(name, implicitly[AttrKey[A]].key), attr)
  def unbindAttr[A <: MaraAttr: AttrKey](name: String): Scope[MaraAttr] = unbind[MaraAttr](attrKey(name, implicitly[AttrKey[A]].key))



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

trait NamespaceLookup {
  def namespace: Namespace

  def lookupValue(name: String): MaraValue = namespace.lookupValue(name)
  def declareValue(name: String): Scope[MaraValue] = namespace.declareValue(name)
  def bindValue(name: String, value: MaraValue): MaraValue = namespace.bindValue(name, value)
  def unbindValue(name: String): Scope[MaraValue] = namespace.unbindValue(name)

  def lookupType(name: String): MaraType = namespace.lookupType(name)
  def declareType(name: String): Scope[MaraType] = namespace.declareType(name)
  def bindType(name: String, typex: MaraType): MaraType = namespace.bindType(name, typex)
  def unbindType(name: String): Scope[MaraType] = namespace.unbindType(name)

  def lookupAttr[A <: MaraAttr: AttrKey](name: String): MaraAttr = namespace.lookupAttr[A](name)
  def declareAttr[A <: MaraAttr: AttrKey](name: String): Scope[MaraAttr] = namespace.declareAttr[A](name)
  def bindAttr[A <: MaraAttr: AttrKey](name: String, attr: A): MaraAttr = namespace.bindAttr[A](name, attr)
  def unbindAttr[A <: MaraAttr: AttrKey](name: String): Scope[MaraAttr] = namespace.unbindAttr[A](name)

  def inNewScope[T](f: => T): T = {
    namespace.pushScope()
    val result = f
    namespace.popScope()
    result
  }

}
