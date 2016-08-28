package io.dac.mara

/**
  * Created by dcollins on 8/28/16.
  */
class FunctionSpec extends MaraSpec with MaraLanguage {
  "Concrete Functions" should "evaluate to a FunctionValue" in {
    eval("def foo(A, B)(x, y) { 10 }") should include("FunctionValue")
  }

  "Abstract Functions" should "evaluate to a FunctionValue" in {
    eval("def foo(A, B)(x, y)") should include("FunctionValue")
  }
}
