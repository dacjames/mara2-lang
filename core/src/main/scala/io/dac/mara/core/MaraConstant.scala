package io.dac.mara.core

sealed trait MaraConstant {
  def name: String
  def ref: String =
    s"@.${name}"

  override def toString = this.ref
}

object MaraConstant {
  case object UnitConstant extends MaraConstant {
    override val name: String = "Unit"
  }
}
