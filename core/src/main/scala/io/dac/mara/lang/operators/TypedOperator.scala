package io.dac.mara.lang.operators

import io.dac.mara.core.MaraType
import io.dac.mara.phases.{Typed, TypedOp}

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedOperator  extends TypedOp with OperatorAlg[Typed] {
  import MaraType._

  private[this] def binop(name: String)(x: Typed, y: Typed)(f: MaraType => Boolean): Typed = op {
    val typex = MaraType.promote(x.typex)
    val typey = MaraType.promote(y.typex)

    if (MaraType.isSubtype(typex, typey) && f(typex)) {
      typex
    } else {
      ErrorType(s"Binary operator ${name} on invalid types ${typex} and ${typey}")
    }
  }

  private[this] def numop(name: String)(x: Typed, y: Typed) = binop(name)(x, y)(t => MaraType.isSubtype(t, IntType()))
  private[this] def boolop(name: String)(x: Typed, y: Typed) = binop(name)(x, y)(t => MaraType.isSubtype(t, BoolType()))


  override def plus(x: Typed, y: Typed): Typed = numop("plus")(x, y)

  override def minus(x: Typed, y: Typed): Typed = numop("minus")(x, y)

  override def times(x: Typed, y: Typed): Typed = numop("times")(x, y)

  override def divide(x: Typed, y: Typed): Typed = numop("divide")(x, y)

  override def power(x: Typed, y: Typed): Typed = numop("power")(x, y)

  override def lt(x: Typed, y: Typed): Typed = ???

  override def gt(x: Typed, y: Typed): Typed = ???

  override def lte(x: Typed, y: Typed): Typed = ???

  override def gte(x: Typed, y: Typed): Typed = ???

  override def ne(x: Typed, y: Typed): Typed = ???

  override def eq(x: Typed, y: Typed): Typed = ???

  override def and(x: Typed, y: Typed): Typed = boolop("and")(x, y)

  override def or(x: Typed, y: Typed): Typed = boolop("or")(x, y)

  override def not(x: Typed): Typed = boolop("not")(x, x)
}
