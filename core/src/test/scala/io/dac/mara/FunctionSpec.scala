package io.dac.mara

/**
  * Created by dcollins on 8/28/16.
  */
class FunctionSpec extends MaraSpec with MaraLanguage {
  "Concrete Functions" should "evaluate to a FunctionValue" in {
    eval("def foo(A, B)(x, y) { if true { x } else { y } }") should include("FunctionValue")
    show("def foo(A, B)(x, y) { if true { x } else { y } }") shouldEqual "def foo(A, B)(x, y) { if true { x } else { y } }"
  }

  it should "have the type of the function body" in {
    typed("def foo(x, y) { 10 }") shouldEqual "FunctionType(RecordType(Vector(InferrableType(), InferrableType())),IntType())"
  }

  it should "support trailing commas" in {
    eval("def foo(x, y, ) { 10 }") should include("FunctionValue")
  }

  it should "type function parameters" in {
    typed("def foo(x: Int, y: Bool) { x }") shouldEqual "FunctionType(RecordType(Vector(TagType('x',IntType()), TagType('y',BoolType()))),IntType())"
  }

  "Abstract Functions" should "evaluate to a FunctionValue" in {
    eval("def foo(A, B)(x, y)") should include("FunctionValue")
  }

  "Function Calls" should "evaluate the result of a Function" in {
    eval("do { def foo(x, y) { x ; y } ; .foo(1, 2) }") should include("IntValue(2)")
  }

  it should "support trailing commas" in {
    eval("do { def foo(x, y) { y } ; .foo(1, 2, ) }") should include("IntValue(2)")
  }

  it should "have the type of the function they are calling" in {
    typed("do { def foo(x: Int, y: Bool) { x }; .foo(1, true) }") shouldEqual "IntType()"
  }

  "Working Examples" should "include fibinocci function" in {
    eval("do { def qua(x) { if x <= 1 { 1 } else { 2 } }; .qua(1) }") should include("IntValue(1)")
    eval("do { def qua(x) { if x <= 1 { 1 } else { 2 } }; .qua(2) }") should include("IntValue(2)")
    eval("do { def fib(x) { if x <= 1 { 1 } else { .self(x-1) + .self(x-2) } }; .fib(5) }") should include("IntValue(8)")
  }

}
