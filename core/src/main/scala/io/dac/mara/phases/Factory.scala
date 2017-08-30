package io.dac.mara.phases

import io.dac.mara.core._

abstract class Node {
  def exec[E](alg: ExprAlg[E]): E
}

trait Factory extends Expr[Factory] {
  override type Target = Node
  override def value: Node = build

  def build: Node
}

object Factory {
  implicit object FactoryPhase extends Phase[Factory] {
    override def key: Int = 4
  }
}

trait FactoryOp extends ExprOps[Factory] {
  override def op(f: => Node) = {
    val index = context.nextIndex[Factory]

    context.put(index)(new Factory {
      override def build = f
      override def get[A <: Expr[A] : Phase]: A#Target = context.get(index)
    })
  }
}



