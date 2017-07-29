package io.dac.mara.core

/**
  * Created by dcollins on 8/12/16.
  */
abstract class Expr[E <: Expr[E]] {
  type Target

  def phase: Phase
}

object Expr {
  trait Replable[A, B] {
    def value(a: A): B
  }
}

abstract class ExprOps[E <: Expr[E]: PhaseKey](implicit val context: PhaseContext)
