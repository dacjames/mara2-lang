package io.dac.mara.core

/**
  * Created by dcollins on 8/12/16.
  */
abstract class Expr[+E <: Expr[E]: Empty] {
  type Target

  def value: Target
  def get[A <: Expr[A]: Phase]: A#Target
  override def toString: String = value.toString
}

abstract class ExprOps[E <: Expr[E]: Phase](implicit val context: TreeContext) {
  def op(f: => E#Target): E = {
    val index = context.nextIndex[E]
    opimpl(f, index)
  }

  def opimpl(f: => E#Target, index: TreeIndex): E

  def opWith[In <: Expr[In]: Phase](f: In#Target => E#Target): E = {
    val index = context.nextIndex[E]
    val input = context.get[In](index)

    opimpl(f(input), index)
  }
}
