package io.dac.mara.lang.app

import io.dac.mara.core._

trait AppAlg[E] extends ExprAlg[E] {
  def app(name: String, args: Seq[Pair.Value], body: => Seq[E]): E = empty
}
