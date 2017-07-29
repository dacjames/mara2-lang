package io.dac.mara.core

/**
  * Created by dcollins on 8/12/16.
  */
abstract class Expr[+E <: Expr[E]] {
  type Target

  def phase: Phase
  def value: Target
}

abstract class ExprOps[E <: Expr[E]: PhaseKey](implicit val context: PhaseContext)
