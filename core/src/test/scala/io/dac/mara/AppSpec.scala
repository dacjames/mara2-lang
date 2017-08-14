package io.dac.mara

import io.dac.mara.phases.Eval

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

  it should "be hackily staged" in {
    fullPipeline[Eval]("do { def add(x: Int, y: Int) { x + y }; app foo(){ .add(1, 2) }; .foo \n}") shouldEqual "IntValue(3)"

    fullPipeline[Eval](
      """
        | def add(x: Int, y: Int) { x + y }
        |
        | def sub(x: Int, y: Int) { x - y }
        |
        | app foo() {
        |   .add(1, .sub(3, 0))
        | }
        |
        | .foo
        |""".stripMargin) shouldEqual "IntValue(4)"
  }
}
