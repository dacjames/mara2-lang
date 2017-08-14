package io.dac.mara.lang.compound

import io.dac.mara.core._

trait CompoundAlg[E] extends ExprAlg[E] {
  def module(exprs: Seq[E]): E
	def dox(block: Seq[E]): E = ???
	def list(exprs: Seq[E]): E = ???
  def record(tags: Seq[(E, E)]): E = ???

  def get(name: String, args: Seq[E]): E = ???
}