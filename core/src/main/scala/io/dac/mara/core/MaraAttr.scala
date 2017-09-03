package io.dac.mara.core

import io.dac.mara.ir.IrModel

sealed trait MaraAttr extends MaraRoot

sealed abstract class AttrKey[A <: MaraAttr] {
  def key: Int
}

object MaraAttr {
  case class ErrorAttr(msg: String) extends MaraAttr
  case class CodeAttr(code: IrModel.Fragment) extends MaraAttr
  case class ExecutableAttr(value: MaraValue) extends MaraAttr
  case class RefAttr(value: IrModel.LVal) extends MaraAttr

  implicit object ErrorKey extends AttrKey[ErrorAttr] {
    override def key: Int = 0
  }
  implicit object CodeKey extends AttrKey[CodeAttr] {
    override def key: Int = 1
  }
  implicit object ValueKey extends AttrKey[ExecutableAttr] {
    override def key: Int = 2
  }
  implicit object RefKey extends AttrKey[RefAttr] {
    override def key: Int = 3
  }
}


