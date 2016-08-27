package io.dac.mara

/**
  * Created by dcollins on 8/20/16.
  */
class LiteralSpec extends MaraSpec with MaraLanguage {
  "Literals" should "show string literals" in {
    show("\"hello, world\"") should be("'hello, world'")
  }

  it should "eval int literals" in {
    eval("10") should be("IntValue(10)")
  }

  it should "eval bool literals" in {
    eval("true") should be("BoolValue(true)")
    eval("false") should be("BoolValue(false)")
  }

  "Boolean Literals" should "have type Bool" in {
    typed("true") should be("BoolType()")
  }

  "String Literals" should "have type String" in {
    typed("'hello, world'") should be("StringType()")
  }

  "Int Literals" should "have type Int" in {
    typed("10") should be ("IntType()")

  }
}
