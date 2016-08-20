package io.dac.mara

import java.io.{PrintWriter, StringWriter}

import io.dac.mara.controlflow.{EvalControlFlow, ShowControlFlow}
import io.dac.mara.core.{Expr, ExprAlg, LangParser}
import io.dac.mara.exprops.{Eval, Show}
import io.dac.mara.literals.{EvalLiteral, ShowLiteral}
import io.dac.mara.operators.{EvalOperator, ShowOperator}
import io.dac.mara.variables.{EvalVariable, ShowVariable}
import org.parboiled2.{ParseError, ParserInput}

import scala.util.{Failure, Success, Try}

/**
  * Created by dcollins on 8/20/16.
  */
trait MaraLanguage {

  import Show._
  import Eval._

  private[this] class C

  private[this] def trace2string(e: Throwable) = {
    val sw = new StringWriter
    e.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  private[this] def run[E <: Expr, Alg <: ExprAlg[E], R](parser: LangParser[E, Alg])(implicit f: Expr.Family[E, R]) =
    parser.Root.run() match {
      case Success(result) => f.value(result).toString
      case Failure(error: ParseError) => parser.formatError(error)
      case Failure(error: Throwable) => trace2string(error)
    }

  def show(text: String)(implicit f: Expr.Family[Show, String]) = run {
    new MaraParser[Show, ShowLiteral with ShowOperator with ShowControlFlow with ShowVariable] {
      val alg = new C with ShowLiteral with ShowOperator with ShowControlFlow with ShowVariable
      val input = ParserInput(text)
    }
  }

  def eval(text: String) = run {
    new MaraParser[Eval, EvalLiteral with EvalOperator with EvalControlFlow with EvalVariable] {
      val alg = new C with EvalLiteral with EvalOperator with EvalControlFlow with EvalVariable
      val input = ParserInput(text)
    }
  }
}
