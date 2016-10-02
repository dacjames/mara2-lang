package io.dac.mara.lang.functions

import scala.collection.mutable
import io.dac.mara.core.{MaraType, MaraValue, Namespace}
import io.dac.mara.exprops.{Eval, EvalOp}

/**
  * Created by dcollins on 8/28/16.
  */
trait EvalFunction extends EvalOp with FunctionAlg[Eval] with Namespace {
  import MaraValue._
  import MaraType._


  override def valparam(name: String, typex: Option[String]): Eval = op {
    ValueParamValue(name, typex.map(lookupType).getOrElse(AnyType()))
  }

  override def typeparam(name: String, typex: Option[String]): Eval = op {
    TypeParamValue(name, typex.map(lookupType).getOrElse(AnyType()))
  }

  private[this] def buildFunction(name: String, typeparams: Seq[Eval], valparams: Seq[Eval], body: Seq[Eval]) = {
    val typeparamValues = typeparams.map(_.eval).collect {
      case it: TypeParamValue => it
    }

    val valparamValues = valparams.map(_.eval).collect {
      case it: ValueParamValue => it
    }


    FunctionValue(name=name, typeparams=typeparamValues, valparams=valparamValues, body=body)
  }

  override def defconcrete(name: String, typeparams: Seq[Eval], valparams: Seq[Eval], typex: Option[String], body: Seq[Eval]): Eval = op {
    bindValue(name, buildFunction(name, typeparams, valparams, body))
  }

  override def defabstract(name: String, typeparams: Seq[Eval], valparams: Seq[Eval], typex: Option[String]): Eval = op {
    bindValue(name, buildFunction(name, typeparams, valparams, Seq.empty[Eval]))
  }

  override def call(name: String, args: Seq[Eval]): Eval = op {
    val func = lookupValue(name)
    func match {
      case FunctionValue(name, typeparams, valparams, body: Seq[Eval]) => {
        val zipped = valparams.zip(args)
        val debug = mutable.ArrayBuffer.empty[String]

        zipped.foreach {
          case (ValueParamValue(name, typex), arg: Eval) => {
            val value = arg.eval
            bindValue(name, value)
            debug.append(s"${name}=${value}")
          }
        }

        bindValue("self", func)

        logger.trace(s".${name}(${debug.mkString(", ")})")

        val result = body.reduce{ (a, b) => {a.eval; b} }.eval

        unbindValue("self")

        zipped.foreach {
          case (ValueParamValue(name, typex), arg: Eval) => {
            unbindValue(name)
          }
        }

        result
      }
      case e: ErrorValue => e
      case _ => ErrorValue(s"${name} is not callable")
    }
  }

}
