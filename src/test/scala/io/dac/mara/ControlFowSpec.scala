package io.dac.mara

/**
  * Created by dcollins on 8/20/16.
  */
class ControlFowSpec extends MaraSpec with MaraLanguage {
  "Control Flow" should "show the if statement" in {
    show("if 1 {1}") should be("if 1 { 1 }")
  }

  it should "evaluate the if statement with true predicate" in {
    eval("if 1 { 3 }") should be("3")
  }

  it should "evaluate the if statement with the false predicate" in {
    eval("if 0 { 2 }") should be("0")
  }

}
