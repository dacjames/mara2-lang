package io.dac.mara.lang.functions

import com.typesafe.scalalogging.LazyLogging
import io.dac.mara.core._

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionAlg[E] extends ExprAlg[E] with LazyLogging {

  def funconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[E]): E = empty

  def funabstract(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String]): E = {
    funconcrete(name, typeparams, valparams, typex, Seq.empty)
  }

  def call(name: String, args: Seq[E]): E = empty
}
