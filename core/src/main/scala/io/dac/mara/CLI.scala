package io.dac.mara

import io.dac.mara.utils.TimeIt
import io.dac.macros.PipelineBuilder
import io.dac.mara.core.Record


/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends MaraLanguage with TimeIt with App {

  val r = Record(1 -> "hello", 2 -> "world")
  val q = Record(1 -> "hello", "qua" -> "wex")
  println(s"R: ${r} ${r.get(1)}, Q: ${q}, ${q.get("qua")}, ${q(1)}")



  for { i <- 1 until 1000} {
    eval("def fib(x) { if x <= 1 { 1 } else { .self(x-1) + .self(x-2) } }")
    eval(".fib(10)")
  }
  timeIt {
    eval("do { .fib(10); .fib(10) }")
  }

  println(litpipeline("5"))


  import PipelineBuilder._

  myassert(true, "Not seen")
  try {
    myassert(false, "Thrown")
  } catch {
    case e: AssertionError => {}
  }

}
