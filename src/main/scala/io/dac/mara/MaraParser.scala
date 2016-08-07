package io.dac.mara

import com.sun.xml.internal.xsom.impl.Ref.Term
import io.dac.mara.literals.{LiteralAlg, LiteralParser}
import io.dac.mara.operators.{OperatorParser, OperatorAlg}
import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E, T <: LiteralAlg[E] with OperatorAlg[E]]
  extends Parser
  with LiteralParser[E, T]
  with OperatorParser[E, T] {

  def InputLine = rule { Expr | Terminal ~ EOI }

  def Expr: Rule1[E] = rule { Operator }

  def Terminal: Rule1[E] = rule { LiteralExpr | Parens }

  def Parens = rule { '(' ~ Expr ~ ')' }

}
