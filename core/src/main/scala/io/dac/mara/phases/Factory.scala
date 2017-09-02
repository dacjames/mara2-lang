package io.dac.mara.phases

import io.dac.mara.core._

abstract class Node {
  def exec[E: Empty](alg: ExprAlg[E]): E
}

case object EmptyNode extends Node {
  override def exec[E: Empty](alg: ExprAlg[E]): E = alg.empty
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

  implicit object FactoryEmpty extends Empty[Factory] {
    override def empty: Factory = ???
  }
}

trait FactoryOp extends ExprOps[Factory] {
  override def opimpl(f: => Node, index: TreeIndex) = {
    context.put(index)(new Factory {
      override def build = f
      override def get[A <: Expr[A] : Phase]: A#Target = context.get(index)
    })
  }
}



