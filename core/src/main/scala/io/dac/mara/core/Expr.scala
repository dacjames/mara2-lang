package io.dac.mara.core

/**
  * Created by dcollins on 8/12/16.
  */
abstract class Expr[+E <: Expr[E]] {
  type Target

  def phase: TreeIndex
  def value: Target
}

abstract class ExprOps[E <: Expr[E]: Phase](implicit val context: TreeContext)
