package io.dac.mara

import java.io.{PrintWriter, StringWriter}

import io.dac.mara.exprops.Eval
import io.dac.mara.impls.{EvalLiteral, EvalOperator}
import org.parboiled2._

import scala.util.{Failure, Success}


class C

/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends App {
  println("Hello, World!")

  def parser(_input: String) = new MaraParser[Eval, EvalLiteral with EvalOperator] {
    val alg = new C with EvalLiteral with EvalOperator
    val input = ParserInput(_input)
  }

  def trace2string(e: Throwable) = {
    val sw = new StringWriter
    e.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  def parse(input: String) = {
    val p = parser(input)
    p.InputLine.run() match {
      case Success(result) => result.eval.toString
      case Failure(error: ParseError) => p.formatError(error)
      case Failure(error: Throwable) => trace2string(error)
    }
  }

  println(parse("3^(7*4)<=3^7*4||1&&1<=1"))

}
