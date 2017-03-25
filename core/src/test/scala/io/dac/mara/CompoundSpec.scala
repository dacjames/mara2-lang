package io.dac.mara

/**
  * Created by dcollins on 3/25/17.
  */
class CompoundSpec extends MaraSpec with MaraLanguage {
  "Do Blocks" should "evaluate their arguments in order and return the last result" in {
    eval("do { 1; 2; 3 } ") shouldEqual "IntValue(3)"
  }
  "Lists" should "evaluate to a Record with integer keys" in {
    eval("[1, 'asdf', 2]") shouldEqual "Record(0: IntValue(1), 1: StringValue(asdf), 2: IntValue(2))"
  }
  it should "support trailing comma" in {
    eval("[1, 'asdf', 2, ]") shouldEqual "Record(0: IntValue(1), 1: StringValue(asdf), 2: IntValue(2))"
  }

  it should "have the correct type" in {
    typed("[1, 'asdf', 2]") shouldEqual "RecordType(Vector(TagType(0,IntType()), TagType(1,StringType()), TagType(2,IntType())))"
  }

  "Records" should "evaluate to a Record with string keys" in {
    eval("[x: 10, y: 'hello']") shouldEqual "Record(x: IntValue(10), y: StringValue(hello))"
  }

  it should "evaluate to a Record with int keys" in {
    eval("[0: 10, 2: 'hello']") shouldEqual "Record(0: IntValue(10), 2: StringValue(hello))"
  }

  it should "not allow keys to be mixed" in {
    eval("[0: 10, x: 'hello']") should include("ErrorValue")
  }

  it should "have the correct type" in {
    typed("[x: 10, y: 'hello']") shouldEqual "RecordType(Vector(TagType('x',IntType()), TagType('y',StringType())))"
  }
}
