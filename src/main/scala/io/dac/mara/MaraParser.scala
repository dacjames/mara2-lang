package io.dac.mara

import com.sun.xml.internal.xsom.impl.Ref.Term
import io.dac.mara.controlflow.{ControlFlowAlg, ControlFlowParser}
import io.dac.mara.literals.{LiteralAlg, LiteralParser}
import io.dac.mara.operators.{OperatorAlg, OperatorParser}
import org.parboiled2._

/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E, T <: LiteralAlg[E] with OperatorAlg[E] with ControlFlowAlg[E]]
  extends Parser
  with LiteralParser[E, T]
  with OperatorParser[E, T]
  with ControlFlowParser[E, T]{

  def InputLine = rule { (Expr | Terminal) ~ EOI }

  def Expr: Rule1[E] = rule { Operator | ControlFlow }

  def Terminal: Rule1[E] = rule { Literal | Parens }

  def Parens = rule { '(' ~ Expr ~ ')' }

}
