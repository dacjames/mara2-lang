package io.dac.mara

/**
  * Created by dcollins on 8/28/16.
  */
class FunctionSpec extends MaraSpec with MaraLanguage {
  "Concrete Functions" should "evaluate to a FunctionValue" in {
    eval("def foo(A, B)(x, y) { if true { x } else { y } }") should include("FunctionValue")
    show("def foo(A, B)(x, y) { if true { x } else { y } }") shouldEqual "def foo(A, B)(x, y) { if true { x } else { y } }"
  }

  "Abstract Functions" should "evaluate to a FunctionValue" in {
    eval("def foo(A, B)(x, y)") should include("FunctionValue")
  }

  "Function Calls" should "evaluate the result of a Function" in {
    eval("do { def foo(x, y) { x ; y } ; .foo(1, 2) }") should include("IntValue(2)")
  }

  "Working Examples" should "include fibinocci function" in {
    eval("do { def qua(x) { if x <= 1 { 1 } else { 2 } }; .qua(1) }") should include("IntValue(1)")
    eval("do { def qua(x) { if x <= 1 { 1 } else { 2 } }; .qua(2) }") should include("IntValue(2)")
  }

}
