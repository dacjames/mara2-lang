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
    val r = Record(0 -> "x", "1" -> "y")
    r.get(0) should ===(Some("x"))
  }

  it should "reject invalid int keys" in {
    Record.construct(1 -> "x", "1" -> "y") shouldBe a[Left[_, _]]
  }

  it should "reject string keys after int keys" in {
    Record.construct(0 -> "x", "a" -> "y", 1 -> "b") shouldBe a[Left[_, _]]
  }

  it should "have a get method returning None if the key does not exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    r.get("x") should ===(None)
  }

  it should "have an apply method returning value if the key exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    r("1") should ===("y")
  }

  it should "have an apply method that throws an exception if the key does not exists" in {
    val r = Record(1 -> "x", "1" -> "y")
    a[NoSuchElementException] shouldBe thrownBy{ r("x") }
  }

  it should "have a nice toString" in {
    s"${Record(0 -> "x", 1 -> "y")}" shouldEqual "Record(0: x, 1: y)"
  }

  it should "support extension" in {
    (Record(0 -> "x", "y" -> "xx") under Record(0 -> "asdf", 1 -> "y")) shouldEqual Record(0 -> "asdf", 1 -> "y")
  }

  it should "support extension #2" in {
    // TODO Consider ordering implications of Record.under
    (Record("x" -> "x", "y" -> "y") under Record("y" -> "yy", "x" -> "xx")) shouldEqual Record("y" -> "yy", "x" -> "xx")
  }

  "Record Keys" should "be required to be a set" in {
    Record.construct("x" -> 1, "y" -> 2, "x" -> 3) shouldBe a[Left[_, _]]
    Record.construct(0 -> 10, 1 -> 20, "x" -> 1) shouldBe a[Right[_, _]]
  }
}
