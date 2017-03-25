package io.dac.mara.core

/**
  * Created by dcollins on 8/12/16.
  */
trait Expr

object Expr {
  trait Replable[A, B] {
    def value(a: A): B
  }
}
