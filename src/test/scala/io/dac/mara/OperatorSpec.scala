package io.dac.mara

/**
  * Created by dcollins on 8/21/16.
  */
class OperatorSpec extends MaraSpec with MaraLanguage {
  "Less than operator" should "work for ints" in {
    eval("1 < 2") should be("BoolValue(true)")
  }

  "Less than operator" should "give an error for non-ints" in {
    eval("1 < 'asdf'") should include("ErrorValue")
  }

}
