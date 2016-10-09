package io.dac.mara.lang.functions

import com.typesafe.scalalogging.LazyLogging
import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangAlg

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionAlg[E <: Expr] extends LangAlg[E] with LazyLogging {

  type Param = (String, Option[String])

  def defconcrete(name: String, typeparams: Seq[Param], valparams: Seq[Param], typex: Option[String], body: Seq[E]): E = ???

  def defabstract(name: String, typeparams: Seq[Param], valparams: Seq[Param], typex: Option[String]): E = {
    defconcrete(name, typeparams, valparams, typex, Seq.empty)
  }

  def call(name: String, args: Seq[E]): E = ???
}
