package io.dac.mara.core

sealed trait MaraAttr extends MaraRoot

object MaraAttr {
  case class ErrorAttr(msg: String) extends MaraAttr
  case class CodeAttr(code: String) extends MaraAttr
}
