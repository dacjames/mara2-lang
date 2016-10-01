package io.dac.mara.lang.functions

import io.dac.mara.core.Expr
import io.dac.mara.lang.root.LangAlg

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionAlg[E <: Expr] extends LangAlg[E] {
  def typeparam(name: String, bounds: Option[String]): E = ???
  def valparam(name: String, typex: Option[String]): E = ???
  def defconcrete(name: String, typeparams: Seq[E], valparams: Seq[E], typex: Option[String], body: Seq[E]): E = ???
  def defabstract(name: String, typeparams: Seq[E], valparams: Seq[E], typex: Option[String]): E = ???
  def call(name: String, args: Seq[E]): E = ???
}
