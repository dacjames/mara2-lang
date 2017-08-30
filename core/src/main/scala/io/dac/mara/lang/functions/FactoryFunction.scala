package io.dac.mara.lang.functions

import io.dac.mara.core._
import io.dac.mara.phases.{Factory, FactoryOp, Node}

trait FactoryFunction extends FactoryOp with FunctionAlg[Factory] {
  case class DefConcrete(name: String,
                         typeparams: Seq[Pair.Type],
                         valparams: Seq[Pair.Value],
                         typex: Option[String],
                         body: Seq[Factory])
    extends Node {
    override def exec[E](alg: ExprAlg[E]): E = alg match {
      case alg: FunctionAlg[E] => alg.funconcrete(name, typeparams, valparams, typex, body.view.map(_.build.exec(alg)))
    }
  }

  case class DefAbstract(name: String,
                         typeparams: Seq[Pair.Type],
                         valparams: Seq[Pair.Value],
                         typex: Option[String])
    extends Node {
    override def exec[E](alg: ExprAlg[E]): E = alg match {
      case alg: FunctionAlg[E] => alg.funabstract(name, typeparams, valparams, typex)
    }
  }

  case class Call(name: String, args: Seq[Factory]) extends Node {
    override def exec[E](alg: ExprAlg[E]): E = alg match {
      case alg: FunctionAlg[E] => alg.call(name, args.view.map(_.build.exec(alg)))
    }
  }


  override def funconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[Factory]): Factory = op {
    DefConcrete(name, typeparams, valparams, typex, body)
  }

  override def funabstract(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String]): Factory = op {
    DefAbstract(name, typeparams, valparams, typex)
  }

  override def call(name: String, args: Seq[Factory]): Factory = op {
    Call(name, args)
  }
}
