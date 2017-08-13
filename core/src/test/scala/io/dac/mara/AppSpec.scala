package io.dac.mara

class AppSpec extends MaraSpec with MaraLanguage {
  "App" should "be parsed" in {
    show("app foo(x: Int) { }") shouldBe "app foo(x: Int) { }"
  }

  it should "have a function type" in {
    typed("app foo(x: Int) { }") should include("FunctionType")
  }

  it should "be able to be compiled" in {
    compiled("app foo() {}") should include("main")
  }
}
