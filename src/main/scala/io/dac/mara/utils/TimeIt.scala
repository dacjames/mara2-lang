package io.dac.mara.utils

/**
  * Created by dcollins on 10/2/16.
  */
trait TimeIt {
  private[this] var isTiming = false

  private[this] def doTimeIt[R](name: String)(block: => R): R = {
    if (isTiming) {
      val t0 = System.nanoTime()
      val result = block
      // call-by-name
      val t1 = System.nanoTime()
      val elapsed = t1 - t0
      println(s"Elapsed Time ${name}: ${elapsed} ns ${elapsed * 1e-3} µs ${elapsed * 1e-6} ms")
      result
    } else {
      block
    }
  }

  private[this] def timingScope[R](block: => R): R = {
    isTiming = true
    val result = block
    isTiming = false
    result
  }

  def timeInner[R](name: String)(block: => R): R =
    doTimeIt(name)(block)

  def timeThe[R](name: String)(block: => R): R = timingScope {
    doTimeIt(name)(block)
  }

  def timeIt[R](block: => R): R = timeThe("")(block)
}
