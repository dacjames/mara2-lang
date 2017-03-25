package io.dac.mara.lang.compound

import io.dac.mara.lang.root.LangAlg

trait CompoundAlg[E] extends LangAlg[E] {
	def empty: E = ???
	def dox(block: Seq[E]): E = ???
	def list(exprs: Seq[E]): E = ???
  def record(tags: Seq[(E, E)]): E = ???
}