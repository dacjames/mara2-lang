package io.dac.mara.lang.compound

import io.dac.mara.lang.literals.{LiteralAlg, LiteralParser}
import io.dac.mara.lang.parsers._
import io.dac.mara.lang.root.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 3/24/17.
  */
trait CompoundParser[E, Alg <: CompoundAlg[E] with LiteralAlg[E]] extends BlockParser[E, Alg] with SepParser with LiteralParser[E, Alg] with IdentifierParser with TupleParser[E, Alg] with LangParser[E, Alg] {

  def Do: Rule1[E] = rule {
    "do" ~ Block ~> { (x: Seq[E]) =>
      alg.dox(x)
    }
  }

  def List: Rule1[E] = rule {
    NonEmptyList | EmptyList
  }

  private[this] def EmptyList: Rule1[E] = rule {
    '[' ~ Whitespace ~ ']' ~> { () => alg.list(Seq.empty) }
  }

  private[this] def NonEmptyList: Rule1[E] = rule {
    '[' ~ Expr ~ zeroOrMore(ListSep ~ Expr) ~ optional(ListSep) ~ ']' ~> {
      (a: E, b: Seq[E]) => alg.list(a +: b)
    }
  }


  def Record: Rule1[E] = rule {
    '[' ~ RecordTag ~ zeroOrMore(ListSep ~ RecordTag) ~ optional(ListSep) ~ ']' ~> {
      (a: (E, E), b: Seq[(E, E)]) => alg.record(a +: b)
    }
  }

  private[this] def RecordTag: Rule1[(E, E)] = rule {
    RecordKey ~ Expr ~> { (a: E, b: E) => (a, b) }
  }

  private[this] def RecordKey: Rule1[E] = rule {
    ((IntLiteral | StringLiteral) ~ Whitespace ~ ":") | Keyword
  }

  private[this] def Keyword: Rule1[E] = rule {
    ValueId ~> {(x: String) => alg.litstring(x) } ~ ":"
  }

  def Empty: Rule1[E] = rule {
    MATCH ~> { () => alg.empty }
  }

  def Get: Rule1[E] = rule {
    ValueId ~ '[' ~ (IntLiteral | Keyword) ~ zeroOrMore(ListSep ~ (IntLiteral | Keyword)) ~ optional(ListSep) ~ ']' ~> {
      (a: String, b: E, c: Seq[E]) => alg.get(a, b +: c)
    }
  }

}
