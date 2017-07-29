package io.dac.mara.exprops

import io.dac.mara.core._
import io.dac.mara.lang.root.LangAlg

abstract class Node {
  def exec[E](alg: LangAlg[E]): E
}

trait Factory extends Expr[Factory] {
  override type Target = Node
  override def value: Node = build

  def build: Node
}

object Factory {
  implicit object FactoryPhaseKey extends PhaseKey[Factory] {
    override def key: Int = 3
  }
}

trait FactoryOp extends ExprOps[Factory] {
  def op(f: => Node) = {
    new Factory {
      override def build = f
      override val phase: Phase = context.nextPhase[Factory]
    }
  }
}



