package io.dac.mara.lang.parsers

import io.dac.mara.core._
import org.parboiled2._

trait PairParser extends Parser with IdentifierParser {
   def Pairs: Rule1[(Seq[Pair.Type], Seq[Pair.Value])] = rule {
    oneOrMore(ValuePairs | TypePairs) ~> { (tuples: Seq[Seq[Pair]]) =>
      val both = tuples.flatten.partition {
        case _: Pair.Type => true
        case _ => false
      }
      (both._1.asInstanceOf[Seq[Pair.Type]], both._2.asInstanceOf[Seq[Pair.Value]])
    }
  }

   def ValuePairs: Rule1[Seq[Pair.Value]] = rule {
    "(" ~ optional(ValuePair) ~ zeroOrMore("," ~ ValuePair) ~ optional(',') ~ ")" ~> {
      (x: Option[Pair.Value], y: Seq[Pair.Value]) => x match {
        case Some(x) => (x +: y)
        case None => y
      }
    }
  }

   def TypePairs: Rule1[Seq[Pair.Type]] = rule {
    "(" ~ optional(TypePair) ~ zeroOrMore("," ~ TypePair) ~ optional(',') ~ ")" ~> {
      (x: Option[Pair.Type], y: Seq[Pair.Type]) => x match {
        case Some(x) => (x +: y)
        case None => y
      }
    }
  }

   def ValuePair: Rule1[Pair.Value] = rule {
    ValueId ~ ValueType.? ~> { (x: String, y: Option[String]) =>
      y match {
        case Some(y) => Pair.Value2(x, y)
        case None => Pair.Value1(x)
      }
    }
  }

   def TypePair: Rule1[Pair.Type] = rule {
    TypeId ~ TypeBounds.? ~> { (x: String, y: Option[String]) =>
      y match {
        case Some(y) => Pair.Type2(x, y)
        case None => Pair.Type1(x)
      }
    }
  }

   def TypeBounds = rule {
    "::" ~ TypeId ~> { (x: String) => x }
  }

   def ValueType = rule {
    ":" ~ TypeId ~> { (x: String) => x }
  }
}
