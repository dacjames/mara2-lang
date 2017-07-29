package io.dac.mara.core

import scala.collection.mutable

case class TreeIndex(index: Int) extends AnyVal

class TreeContext {
  implicit private def wrapPhase: Int => TreeIndex = TreeIndex
  implicit private def unwrapPhase: TreeIndex => Int = _.index

  private[this] val phases: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty

  def nextIndex[E <: Expr[E]: Phase]: TreeIndex = {
    val key = implicitly[Phase[E]].key

    for (_ <- phases.size until key + 1) {
      phases += 0
    }

    val next = phases(key)
    phases.update(key, next + 1)
    next
  }
}
