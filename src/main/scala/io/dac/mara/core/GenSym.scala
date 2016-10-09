package io.dac.mara.core

/**
  * Created by dcollins on 10/8/16.
  */
trait GenSym {
  private[this] var typeSymNum = 0
  private[this] var valueSymNum = 0

  def typeSym() = {
    val sym = s"T${typeSymNum}"
    typeSymNum += 1
    sym
  }

  def valueSym() = {
    val sym = s"v${valueSymNum}"
    valueSymNum += 1
    sym
  }

}
