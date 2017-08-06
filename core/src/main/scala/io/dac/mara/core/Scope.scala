package io.dac.mara.core

import scala.collection.{Map, mutable}

trait ScopeLike[A, B] extends mutable.Map[A, B] with mutable.MapLike[A, B, ScopeLike[A, B]]  {
  protected val parentOpt: Option[ScopeLike[A, B]] = None
  private[this] var space: mutable.Map[A, B] = mutable.Map.empty

  override def +=(kv: (A, B)) = {
    space += kv
    this
  }

  override def -=(key: A): ScopeLike.this.type = {
    space -= key
    this
  }

  override def get(key: A): Option[B] = {
    val inSpace = space.get(key)
    inSpace match {
      case Some(_) => inSpace
      case None => parentOpt match {
        case Some(parent) => parent.get(key)
        case None => None
      }
    }
  }

  override def iterator: Iterator[(A, B)] = parentOpt match {
    case None => space.iterator
    case Some(parent) =>
      new Iterator[(A, B)] {
        private[this] val spaceIter = space.iterator
        private[this] val parentIter = parent.iterator
        override def hasNext: Boolean = spaceIter.hasNext || parentIter.hasNext
        override def next(): (A, B) =
          if (spaceIter.hasNext) { spaceIter.next() }
          else { parentIter.next() }
      }
  }

  override def seq: mutable.Map[A, B] = parentOpt match {
    case None => space.seq
    case Some(parent) => parent.seq ++ space.seq
  }

  override def empty: ScopeLike[A, B] = this
}


class Scope[Kind](parent: Option[Scope[Kind]]) extends ScopeLike[String, Option[Kind]] {
  override val parentOpt = parent
  def push: Scope[Kind] = new Scope(Some(this))
  def pop: Option[Scope[Kind]] = parentOpt
}
object Scope {
  def empty[Kind <: MaraRoot] = new Scope[Kind](None)
}
