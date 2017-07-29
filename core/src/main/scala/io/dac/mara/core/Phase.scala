package io.dac.mara.core

import scala.collection.mutable

case class Phase(index: Int) extends AnyVal

abstract class PhaseKey[A] {
  def key: Int
}


class PhaseContext {
  implicit private def wrapPhase: Int => Phase = Phase
  implicit private def unwrapPhase: Phase => Int = _.index

  private[this] val phasePosition: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty

  def nextPhase[E <: Expr[E]: PhaseKey]: Phase = {
    val key = implicitly[PhaseKey[E]].key

    for (_ <- phasePosition.size until key + 1) {
      phasePosition += 0
    }

    val next = phasePosition(key)
    phasePosition.update(key, next + 1)
    next
  }
}
