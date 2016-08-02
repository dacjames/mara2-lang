package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
trait ShowArithmetic extends ShowOp with ArithmeticAlg[Show] {
  override def plus(x: Show, y: Show): Show = op { s"(${x.show} + ${y.show})" }
  override def minus(x: Show, y: Show): Show = op { s"(${x.show} - ${y.show})" }
  override def times(x: Show, y: Show): Show = op { s"(${x.show} * ${y.show})" }
  override def divide(x: Show, y: Show): Show = op { s"(${x.show} / ${y.show})" }
  override def power(x: Show, y: Show): Show = op { s"(${x.show} ^ ${y.show})" }
}
