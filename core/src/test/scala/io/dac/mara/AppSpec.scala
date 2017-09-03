package io.dac.mara

import io.dac.mara.phases.Eval

class AppSpec extends MaraSpec with MaraLanguage {
  "App" should "be parsed" in fresh {
    show("app foo(x: Int) { }") shouldBe "app foo(x: Int) { }"
  }

  it should "have a function type" in fresh {
    typed("app foo(x: Int) { }") should include("FunctionType")
  }

  it should "be able to be compiled" in fresh {
    compiled("app foo() {}") should include("main")
  }

  it should "be hackily staged" in fresh {
    fullPipeline[Eval]("do { fun add(x: Int, y: Int) { x + y }; app foo(){ .add(1, 2) }; .foo \n}") shouldEqual "IntValue(3)"

    fullPipeline[Eval](
      """
        | fun add(x: Int, y: Int) { x + y }
        |
        | fun sub(x: Int, y: Int) { x - y }
        |
        | app foo() {
        |   .add(1, .sub(3, 0))
        | }
        |
        | .foo
        |""".stripMargin) shouldEqual "IntValue(4)"
  }
}
