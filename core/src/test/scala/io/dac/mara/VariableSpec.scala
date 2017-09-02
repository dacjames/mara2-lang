package io.dac.mara

import io.dac.mara.phases.{Compiled, Eval}

/**
  * Created by dcollins on 8/27/16.
  */
class VariableSpec extends MaraSpec with MaraLanguage {
  "Variables assignment" should "parse without types" in {
    eval("val x = 10") should be("IntValue(10)")
  }

  it should "support pipeline" in {
    pipeline("val x = 10") should be("val x = 10 :: 10 ==> IntValue(10)")
  }

  it should "parse correctly with types" in {
    eval("val x: Bool = true") should be("BoolValue(true)")
  }

  it should "parse correctly in a block" in {
    eval("do { val x = 'hello'; val y = 'world'; y }") should be("StringValue(world)")
  }

  it should "have the type of the value" in {
    typed("val x = 10") should be("10")
  }

  it should "be a type error if the declared type does not match the value type" in {
    typed("val x: Bool = 'hello, world'") should include("ErrorType")
  }

  "Variable substition" should "have the type of the value" in {
    typed("do {val x: Int; x}") should be("IntType()")
  }

  it should "work with operators" in {
    eval("do { val x = 10; x + 2 }") should be("IntValue(12)")
  }

  it should "be compiled" in fresh {
    fullPipeline[Compiled]("val x = 10") should be("%t0 = alloca i32\nstore i32 10, i32* %t0\n%t1 = load i32, i32* %t0")
  }

  it should "assign and access variables in compiled code" in fresh {
    fullPipeline[Eval](
      """
        | fun asdf() {
        |   val z = 4
        |   z
        | }
        |
        | app main() {
        |   .asdf
        | }
        |
        | .main
      """.stripMargin) shouldBe "IntValue(4)"
  }
}