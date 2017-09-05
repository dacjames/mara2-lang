package io.dac.mara.lang.operators

import io.dac.mara.core.MaraType
import io.dac.mara.phases.{Typed, TypedOp}

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedOperator  extends TypedOp with OperatorAlg[Typed] {
  import MaraType._

  private[this] def binop(name: String)(x: Typed, y: Typed)(result: MaraType): Typed = op {
    import MaraType._

    val a = x.typex
    val b = y.typex

    val result =
      if (a == b) BinopType(a, b, a)
      else if (isSubtype(a, b)) BinopType(b, b, b)
      else if (isSubtype(b, a)) BinopType(a, a, a)
      else if (isSubtype(a, promote(b))) BinopType(promote(a), promote(b), promote(b))
      else if (isSubtype(b, promote(a))) BinopType(promote(a), promote(b), promote(a))
      else ErrorType(s"$a and $b are not compatible for binary operations")

    result match {
      case e: ErrorType => e
      case bin: BinopType =>
        if (isSubtype(bin.output, result)) bin
        else ErrorType(s"Binary operator ${name} must have result type ${result}")
    }

  }

  private[this] def numop(name: String)(x: Typed, y: Typed) = binop(name)(x, y)(IntType())
  private[this] def boolop(name: String)(x: Typed, y: Typed) = binop(name)(x, y)(BoolType())


  override def plus(x: Typed, y: Typed): Typed = numop("plus")(x, y)

  override def minus(x: Typed, y: Typed): Typed = numop("minus")(x, y)

  override def times(x: Typed, y: Typed): Typed = numop("times")(x, y)

  override def divide(x: Typed, y: Typed): Typed = numop("divide")(x, y)

  override def power(x: Typed, y: Typed): Typed = numop("power")(x, y)

  override def lt(x: Typed, y: Typed): Typed = op { MaraType.BoolType() }

  override def gt(x: Typed, y: Typed): Typed = ???

  override def lte(x: Typed, y: Typed): Typed = ???

  override def gte(x: Typed, y: Typed): Typed = ???

  override def ne(x: Typed, y: Typed): Typed = ???

  override def eq(x: Typed, y: Typed): Typed = ???

  override def and(x: Typed, y: Typed): Typed = boolop("and")(x, y)

  override def or(x: Typed, y: Typed): Typed = boolop("or")(x, y)

  override def not(x: Typed): Typed = boolop("not")(x, x)
}
