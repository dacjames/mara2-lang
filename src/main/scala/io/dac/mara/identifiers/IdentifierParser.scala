package io.dac.mara.identifiers

import io.dac.mara.core.{Expr, LangParser}
import io.dac.mara.variables.VariableAlg
import org.parboiled2.CharPredicate

/**
  * Created by dcollins on 8/13/16.
  */
trait IdentifierParser[E <: Expr, T <: VariableAlg[E]] extends LangParser[E, T]  {
  def ValueId = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.AlphaNum))
  }

  def TypeId = rule {
    capture(zeroOrMore("_") ~ oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum))
  }
}
