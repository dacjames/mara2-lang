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

  "Boolean Literals" should "have literal types" in {
    typed("true") should be("true")
  }

  "String Literals" should "have literal types" in {
    typed("'hello, world'") should be("'hello, world'")
  }

  "Int Literals" should "have type literal int types" in {
    typed("10") should be ("10")

  }
}
