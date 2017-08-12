package io.dac.mara.lang.functions

import com.typesafe.scalalogging.LazyLogging
import io.dac.mara.core._

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionAlg[E] extends ExprAlg[E] with LazyLogging {

  def defconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[E]): E = ???

  def defabstract(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String]): E = {
    defconcrete(name, typeparams, valparams, typex, Seq.empty)
  }

  def call(name: String, args: Seq[E]): E = ???
}
