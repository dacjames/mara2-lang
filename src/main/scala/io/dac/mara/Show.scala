package io.dac.mara

/**
  * Created by dcollins on 8/2/16.
  */
trait Show {
  def show: String
}

trait ShowOp {
  def op(f: => String) =
    new Show {
      def show = f
    }
}
