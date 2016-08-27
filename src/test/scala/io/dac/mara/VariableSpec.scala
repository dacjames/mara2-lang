package io.dac.mara

/**
  * Created by dcollins on 8/27/16.
  */
class VariableSpec extends MaraSpec with MaraLanguage {
  "Variables assignment" should "have the type of the value" in {
    typed("val x=10") should be ("IntType()")
  }

  it should "be a type error if the declared type does not match the value type" in {
    typed("val x:Bool='hello, world'") should include("TypeError")
  }
}
