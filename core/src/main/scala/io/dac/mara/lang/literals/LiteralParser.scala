package io.dac.mara.lang.literals

import io.dac.mara.lang.parsers.LiteralSyntaxParser
import io.dac.mara.lang.root.LangParser
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait LiteralParser[E, Alg <: LiteralAlg[E]] extends LiteralSyntaxParser with LangParser[E, Alg] {
  def Literal: Rule1[E] = rule { ActualLiteral ~ Whitespace }

  private[this] def ActualLiteral = rule { StringLiteral | IntLiteral | BoolLiteral }

  def IntLiteral = rule {
    IntLiteralSyntax ~> { x: Int => alg.litint(x) }
  }

  def StringLiteral = rule {
    StringLiteralSyntax ~> { x: String => alg.litstring(x) }
  }

  private[this] def BoolLiteral = rule {
    capture("true" | "false") ~> {  x: String =>
      if (x.trim == "true") { alg.litbool(true) }
      else { alg.litbool(false) }
    }
  }
}