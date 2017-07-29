package io.dac.mara.lang.controlflow

import io.dac.mara.exprops.{Factory, FactoryOp, Node}
import io.dac.mara.lang.root.LangAlg

trait FactoryControlFlow extends FactoryOp with ControlFlowAlg[Factory] {

  case class If(pred: Factory, body: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: ControlFlowAlg[E] => alg.ifx(pred.build.exec(alg), body.build.exec(alg))
    }
  }

  case class Else(pred: Factory, body: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: ControlFlowAlg[E] => alg.elsex(pred.build.exec(alg), body.build.exec(alg))
    }
  }

  override def ifx(pred: Factory, body: Factory): Factory = op {
    If(pred, body)
  }

  override def elsex(expr: Factory, otherwise: Factory): Factory = op {
    Else(expr, otherwise)
  }
}
