package io.dac.mara

import com.sun.xml.internal.xsom.impl.Ref.Term
import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E, T <: LiteralAlg[E] with ArithmeticAlg[E]]
  extends Parser
  with LiteralParser[E, T]
  with ArithmeticParser[E, T] {

  def InputLine = rule { Expr | TerminalExpr ~ EOI }

  def Expr: Rule1[E] = rule { Parens | ArithmeticExpr }

  def TerminalExpr: Rule1[E] = rule { LiteralExpr }

}
