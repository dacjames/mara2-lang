package io.dac.mara

/**
  * Created by dcollins on 8/20/16.
  */
class ControlFowSpec extends MaraSpec with MaraLanguage {
  "Control Flow" should "show the if statement" in {
    show("if 1 {1}") should be("if 1 { 1 }")
  }

  it should "evaluate the if statement with trueish predicate" in {
    eval("if 1 { 3 }") should be("IntValue(3)")
  }

  it should "evaluate the if statement with the falseish predicate" in {
    eval("if 0 { 2 }") should be("UnitValue()")
  }

  it should "evaluate the if statement with the true predicate" in {
    eval("if true { 2 }") should be("IntValue(2)")
  }

  it should "evaluate the if statement with a false predicate" in {
    eval("if false { 2 }") should be("UnitValue()")
  }

  it should "evaluate blocks" in {
    eval("do { val x = 1; x }") should be("IntValue(1)")
    eval("do { val x = 0; x + 2 }") should be("IntValue(2)")
  }

  it should "evaluate with whitespace" in {
    eval("do { val x = 1\n x }") should be("IntValue(1)")
    eval("do { val x = 0; val y = 2\n x + 2 }") should be("IntValue(2)")
    eval(
      """do {
        |  val x = 1
        |  x
        |}""".stripMargin) should be("IntValue(1)")
    typed(
      """do {
        |  val x = 1
        |  x
        |}""".stripMargin) should be("IntType()")
  }

  it should "honor block scope in if blocks" in {
    eval("do { val x = 1; val y = if true { val x = 10 }; x}") should be("IntValue(1)")
    eval("do { val x = 1; val y = if true { val x = 10 }; y}") should be("IntValue(10)")
  }

  it should "honor block scope in else blocks" in {
    eval("do { val x = 1; val y = if false { 0 } else { val x = 10 }; x }") should be("IntValue(1)")
    eval("do { val x = 1; val y = if false { 0 } else { val x = 10 }; y }") should be("IntValue(10)")
  }

}
