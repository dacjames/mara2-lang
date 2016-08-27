package io.dac.mara.lang.operators

import io.dac.mara.exprops.{Typed, TypedOp}

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedOperator  extends TypedOp with OperatorAlg[Typed] {
  override def plus(x: Typed, y: Typed): Typed = ???

  override def minus(x: Typed, y: Typed): Typed = ???

  override def times(x: Typed, y: Typed): Typed = ???

  override def divide(x: Typed, y: Typed): Typed = ???

  override def power(x: Typed, y: Typed): Typed = ???

  override def lt(x: Typed, y: Typed): Typed = ???

  override def gt(x: Typed, y: Typed): Typed = ???

  override def lte(x: Typed, y: Typed): Typed = ???

  override def gte(x: Typed, y: Typed): Typed = ???

  override def ne(x: Typed, y: Typed): Typed = ???

  override def and(x: Typed, y: Typed): Typed = ???

  override def or(x: Typed, y: Typed): Typed = ???

  override def not(x: Typed): Typed = ???
}
