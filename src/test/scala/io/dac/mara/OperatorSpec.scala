package io.dac.mara

/**
  * Created by dcollins on 8/21/16.
  */
class OperatorSpec extends MaraSpec with MaraLanguage {
  "Plus operator" should "have the type Int" in {
    typed("1 + 2") should be("IntType()")
  }

  it should "give a type error for non-int arguments" in {
    typed("1 + true") should include("TypeError")
  }

  "And operator" should "have type Bool" in {
    typed("true && false") should be("BoolType()")
  }

  it should "give a type error for non-bool arguments" in {
    typed("true && 1") should include("TypeError")
  }

  "Less than operator" should "work for ints" in {
    eval("1 < 2") should be("BoolValue(true)")
  }

  "Less than operator" should "give an error for non-ints" in {
    eval("1 < 'asdf'") should include("ErrorValue")
  }

}
