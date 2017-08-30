package io.dac.mara.core

/**
  * Created by dcollins on 8/12/16.
  */
abstract class Expr[+E <: Expr[E]] {
  type Target

  def value: Target
  def get[A <: Expr[A]: Phase]: A#Target
  override def toString: String = value.toString
}

abstract class ExprOps[E <: Expr[E]: Phase](implicit val context: TreeContext) {
  def op(f: => E#Target): E
}
