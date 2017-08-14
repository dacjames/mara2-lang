package io.dac.mara

import io.dac.mara.lang.app.AppParser
import io.dac.mara.lang.compound.CompoundParser
import io.dac.mara.lang.controlflow.ControlFlowParser
import io.dac.mara.lang.functions.FunctionParser
import io.dac.mara.lang.literals.LiteralParser
import io.dac.mara.lang.operators.OperatorParser
import io.dac.mara.lang.parsers.ParensParser
import io.dac.mara.lang.variables.VariableParser
import org.parboiled2._


/**
  * Created by dcollins on 8/2/16.
  */
trait MaraParser[E, Alg <: lang.CombinedAlg[E]  ]
  extends Parser with LiteralParser[E, Alg]
    with OperatorParser[E, Alg]
    with ControlFlowParser[E, Alg]
    with VariableParser[E, Alg]
    with ParensParser[E, Alg]
    with FunctionParser[E, Alg]
    with CompoundParser[E, Alg]
    with AppParser[E, Alg] {

  def Root: Rule1[E] = File

  def File: Rule1[E] = rule {
    zeroOrMore(ExprSep) ~ Expr ~ zeroOrMore(oneOrMore(ExprSep) ~ Expr) ~ zeroOrMore(ExprSep) ~ EOI ~> { (a: E, b: Seq[E]) =>
      alg.module(a +: b)
    }
  }

  def Expr: Rule1[E] = rule {
    Operator | Terminal
  }

  def Terminal: Rule1[E] = rule {
    Parens | Literal | App | Do | ControlFlow | Function | Get | Variable | Substitution | Call  | List | Record
  }

}

