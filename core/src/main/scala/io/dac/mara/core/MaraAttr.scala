package io.dac.mara.core

sealed trait MaraAttr extends MaraRoot

sealed abstract class AttrKey[A <: MaraAttr] {
  def key: Int
}

object MaraAttr {
  case class ErrorAttr(msg: String) extends MaraAttr
  case class CodeAttr(code: String) extends MaraAttr
  case class ExecutableAttr(value: MaraValue) extends MaraAttr

  implicit object ErrorKey extends AttrKey[ErrorAttr] {
    override def key: Int = 0
  }
  implicit object CodeKey extends AttrKey[CodeAttr] {
    override def key: Int = 1
  }
  implicit object ValueKey extends AttrKey[ExecutableAttr] {
    override def key: Int = 2
  }
}


