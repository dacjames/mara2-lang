package io.dac.mara.lang.app

import io.dac.mara.core._
import io.dac.mara.phases.{Factory, FactoryOp, Node}

trait FactoryApp extends FactoryOp with AppAlg[Factory] {
  case class App(name: String, args: Seq[Pair.Value], body: Seq[Factory]) extends Node {
    override def exec[E](alg: ExprAlg[E]): E = alg match {
      case alg: AppAlg[E] => alg.app(name, args, body.view.map(_.build.exec(alg)))
    }
  }

  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Factory]): Factory = op {
    App(name, args, body)
  }

}
