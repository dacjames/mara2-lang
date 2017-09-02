package io.dac.mara.core
import scala.collection.mutable

case class TreeIndex(index: Int) extends AnyVal

class TreeContext {
  implicit private def wrapPhase: Int => TreeIndex = TreeIndex
  implicit private def unwrapPhase: TreeIndex => Int = _.index

  private[this] val phases: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer.empty
  private[this] val storage: mutable.HashMap[Int, mutable.HashMap[TreeIndex, Any]] = mutable.HashMap.empty

  def nextIndex[E <: Expr[E]: Phase]: TreeIndex = {
    val key = implicitly[Phase[E]].key


    for (_ <- phases.size until key + 1) {
      phases += 0
    }

    val next = phases(key)
    phases.update(key, next + 1)
    next
  }

  def get[E <: Expr[E]: Phase](index: TreeIndex): E#Target = {
    val phaseKey = implicitly[Phase[E]].key

    val phaseStorage = storage(phaseKey)
    phaseStorage(index).asInstanceOf[E].value
  }

  def put[E <: Expr[E]: Phase](index: TreeIndex)(expr: E): E  = {
    val phaseKey = implicitly[Phase[E]].key

    if (storage.contains(phaseKey)) {
      storage(phaseKey).put(index, expr)
    } else {
      storage.put(phaseKey, mutable.HashMap(index -> expr))
    }

    expr
  }

  def clear(): Unit = {
    phases.clear()
    storage.clear()
  }
}
