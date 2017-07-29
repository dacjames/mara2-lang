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
    eval(".foo(x) { 1 + 2 + 3 }")
  }
  timeIt {
    eval("do { .foo; .foo }")
  }

  println(factory("10"))

  println(eval("10"))

  println(factory("if x < 0 { 1 + 2 } else { 3 }"))

}
