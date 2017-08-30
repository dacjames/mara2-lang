package io.dac.mara.lang.functions

import io.dac.mara.core.MaraAttr.{ErrorAttr, ExecutableAttr}
import io.dac.mara.core._
import io.dac.mara.phases.{Eval, EvalOp}

import scala.collection.mutable
import scala.sys.process.Process

/**
  * Created by dcollins on 8/28/16.
  */
trait EvalFunction extends EvalOp with FunctionAlg[Eval] with NamespaceLookup {
  import MaraValue._
  import io.dac.mara.core.MaraType._


  private[this] def buildFunction(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], body: Seq[Eval]) = {

    val typeparamValues = typeparams.map {
      case Pair.Type1(name) => TypeParamValue(name, AnyType())
      case Pair.Type2(name, qualifier) => TypeParamValue(name, lookupType(qualifier))
    }

    val valparamValues = valparams.map {
      case Pair.Value1(name) => ValueParamValue(name, AnyType())
      case Pair.Value2(name, qualifier) => ValueParamValue(name, lookupType(qualifier))
    }


    FunctionValue(name=name, typeparams=typeparamValues, valparams=valparamValues, body=body)
  }

  override def funconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[Eval]): Eval = op {
    bindValue(name, buildFunction(name, typeparams, valparams, body))
  }

  override def call(name: String, args: Seq[Eval]): Eval = op {
    val func = lookupValue(name)
    func match {
      case FunctionValue(name, typeparams, valparams, body: Seq[Eval] @unchecked) => {
        val zipped = valparams.zip(args)
        val debug = mutable.ArrayBuffer.empty[String]

        inNewScope {
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

          // Redundant, but make Intellij happy
          result : MaraValue
        }
      }
      case ExecutableValue(name, path) =>
        val output = Process(path).!
        IntValue(output)
      case e: ErrorValue => e
      case _ => ErrorValue(s"${name} is not callable")
    }
  }

}
