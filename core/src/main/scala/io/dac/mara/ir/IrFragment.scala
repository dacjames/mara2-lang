package io.dac.mara.ir

/**
  * Created by dcollins on 4/30/17.
  */
class IrFragment(val s: String) extends AnyVal {
  def /|(that: IrFragment): Vector[IrFragment] = Vector(this, that)
  override def toString: String = s.toString
}

class IrFragmentVector(val v: Vector[IrFragment]) extends AnyVal {
  def /|(that: IrFragment): Vector[IrFragment] = this.v :+ that
  def +|(that: IrFragment): Vector[IrFragment] = this.v :+ that
}

object IrFragmentVector {
  def apply(s: String): IrFragmentVector = {
    new IrFragmentVector(
      s.split("\n").map(new IrFragment(_)).toVector
    )
  }
}
