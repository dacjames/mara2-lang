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
    override def key: Int = 3
  }
}

trait FactoryOp extends ExprOps[Factory] {
  def op(f: => Node) = {
    new Factory {
      override def build = f
      override val phase: TreeIndex = context.nextIndex[Factory]
    }
  }
}



