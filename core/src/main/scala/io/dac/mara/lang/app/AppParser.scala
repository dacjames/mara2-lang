package io.dac.mara.lang.app

import io.dac.mara.core._
import io.dac.mara.lang.parsers.{BlockParser, IdentifierParser, LangParser, PairParser}
import org.parboiled2._

trait AppParser [E, Alg <: AppAlg[E]] extends LangParser[E, Alg] with PairParser with IdentifierParser with BlockParser[E, Alg] {
  def App: Rule1[E] = rule {
    "app" ~ ValueId ~ ValuePairs ~ Block ~> {
      (name: String, args: Seq[Pair.Value], body: Seq[E]) =>
        alg.app(name, args, body)
    }
  }
}
