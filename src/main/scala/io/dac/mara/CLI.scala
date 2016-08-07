package io.dac.mara

import java.io.{PrintWriter, StringWriter}

import io.dac.mara.controlflow.EvalControlFlow
import io.dac.mara.exprops.Eval
import io.dac.mara.literals.EvalLiteral
import io.dac.mara.operators.EvalOperator
import org.parboiled2._

import scala.util.{Failure, Success}


class C

/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends App {
  println("Hello, World!")

  def parser(_input: String) = new MaraParser[Eval, EvalLiteral with EvalOperator with EvalControlFlow] {
    val alg = new C with EvalLiteral with EvalOperator with EvalControlFlow
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
  println(parse("3+~1+1"))
  println(parse("if1{3}"))
  println(parse("if0{3}else{4}"))

}
