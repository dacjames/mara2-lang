package io.dac.mara

import io.dac.mara.core.Record

/**
  * Created by dcollins on 8/21/16.
  */
class RecordSpec extends MaraSpec {

  "Records" should "have a constructor that accepts int keys" in {
    val r = Record(1 -> "Hello")
    r shouldBe a[Record[_]]
  }

  it should "have a constructor that accepts string keys" in {
    val r = Record("hello" -> "World")
    r shouldBe a[Record[_]]
  }

  it should "have a constructor that accepts mixed keys" in {
    val r = Record(1 -> "asdf", "x" -> "y")
    r shouldBe a[Record[_]]
  }

  it should "have a get method returning Some(value) if the key exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    r.get(1) should ===(Some("x"))
  }

  it should "have a get method returning None if the key does not exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    r.get("x") should ===(None)
  }

  it should "have an apply method returning value if the key exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    r("1") should ===("y")
  }

  it should "have an apply method thowing an exception if the key does not exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    a[NoSuchElementException] shouldBe thrownBy{ r("x") }
  }

  it should "have a nice toString" in {
    s"${Record(0 -> "x", 1 -> "y")}" shouldEqual "Record(0: x, 1: y)"
  }


}
