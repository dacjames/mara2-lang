package io.dac.mara.core

/**
  * Created by dcollins on 8/2/16.
  */
trait ExprAlg[E] {
  // Needed because traits cannot take parameters
  implicit val emptyEvidence: Empty[E]
  def empty = emptyEvidence.empty
}
