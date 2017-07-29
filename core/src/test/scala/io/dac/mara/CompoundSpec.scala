package io.dac.mara

/**
  * Created by dcollins on 3/25/17.
  */
class CompoundSpec extends MaraSpec with MaraLanguage {
  "Do Blocks" should "evaluate their arguments in order and return the last result" in {
    eval("do { 1; 2; 3 } ") shouldEqual "IntValue(3)"
  }

  it should "support pipeline" in {
    pipeline("do { 1; 2; 3}") should be("do {1; 2; 3} :: 3 ==> IntValue(3)")
  }

  "Lists" should "evaluate to a Record with integer keys" in {
    eval("[1, 'asdf', 2]") shouldEqual "Record(0: IntValue(1), 1: StringValue(asdf), 2: IntValue(2))"
  }
  it should "support trailing comma" in {
    eval("[1, 'asdf', 2, ]") shouldEqual "Record(0: IntValue(1), 1: StringValue(asdf), 2: IntValue(2))"
  }

  it should "have a record type" in {
    typed("[1, 'asdf', 2]") shouldEqual "RecordType(0: IntType(), 1: StringType(), 2: IntType())"
  }


  "Records" should "evaluate to a Record with string keys" in {
    eval("[x: 10, y: 'hello']") shouldEqual "Record(x: IntValue(10), y: StringValue(hello))"
  }

  it should "evaluate to a Record with int keys" in {
    eval("[0: 10, 1: 'hello']") shouldEqual "Record(0: IntValue(10), 1: StringValue(hello))"
  }

  it should "reject to a Record with invalid int keys" in {
    eval("[0: 10, 2: 'hello']") should include("ErrorValue")
  }

  it should "allow keys to be mixed so long as " in {
    eval("[0: 10, x: 'hello']") shouldEqual "Record(0: IntValue(10), x: StringValue(hello))"
  }


  it should "support lookup by key" in {
    eval("do { val rec = [x: 1, y: 0]; rec[x:] }") shouldEqual "IntValue(1)"
  }

  it should "support lookup by position" in {
    eval("do { val rec = [x: 1, y: 0]; rec[1] }") shouldEqual "IntValue(0)"
  }

  it should "support lookup by multiple keys" in {
    eval("do { val rec = [x: 1, y: 0]; rec[y:, x:] }") shouldEqual "Record(y: IntValue(0), x: IntValue(1))"
  }

  it should "support lookup by multiple positions" in {
    eval("do { val rec = [x: 1, y: 0]; rec[1, 0] }") shouldEqual "Record(0: IntValue(0), 1: IntValue(1))"
  }

  it should "support lookup by mixed keys" in {
    eval("do { val rec = [x: 1, y: 0]; rec[1, x:] }") shouldEqual "Record(0: IntValue(0), x: IntValue(1))"
  }

  "Record Types" should "support string keys" in {
    typed("[x: 10, y: 'hello']") shouldEqual "RecordType(x: IntType(), y: StringType())"
  }
  it should "support int keys" in {
    typed("[0: 10, 1: 'hello']") shouldEqual "RecordType(0: IntType(), 1: StringType())"
  }

  it should "support mixed keys" in {
    typed("[0: 10, y: 'hello']") shouldEqual "RecordType(0: IntType(), y: StringType())"
  }


}
