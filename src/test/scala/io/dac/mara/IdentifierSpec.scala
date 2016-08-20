package io.dac.mara

/**
  * Created by dcollins on 8/20/16.
  */
class IdentifierSpec extends MaraSpec with MaraLanguage {
  "Identifiers" should "show value identifiers" in {
    show("id") should be("id")
  }

  it should "show type identifiers" in {
    show("FancyClass") should be("FancyClass")
  }
}
