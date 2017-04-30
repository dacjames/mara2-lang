package io.dac.mara

/**
  * Created by dcollins on 4/30/17.
  */
package object ir {
  object implicits {
    implicit def stringToLllvmIr(s: String): IrFragment = new IrFragment(s)
    implicit def irVecToLlvmVecIr(v: Vector[IrFragment]): IrFragmentVector = new IrFragmentVector(v)
  }
}
