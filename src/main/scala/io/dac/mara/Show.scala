package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
case class Show(show: String)

trait ShowOp {
  def op(f: => String) = Show(f)
}
