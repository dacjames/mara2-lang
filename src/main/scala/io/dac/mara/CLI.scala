package io.dac.mara

import java.io.{PrintWriter, StringWriter}

import io.dac.mara.controlflow.{EvalControlFlow, ShowControlFlow}
import io.dac.mara.exprops.{Eval, Show}
import io.dac.mara.literals.{EvalLiteral, ShowLiteral}
import io.dac.mara.operators.{EvalOperator, ShowOperator}
import io.dac.mara.variables.{EvalVariable, ShowVariable}
import org.parboiled2._

import scala.util.{Failure, Success}


class C

/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends App {
  println("Hello, World!")

  def parser2(_input: String) = new MaraParser[Show, ShowLiteral with ShowOperator with ShowControlFlow with ShowVariable] {
    val alg = new C with ShowLiteral with ShowOperator with ShowControlFlow with ShowVariable
    val input = ParserInput(_input)
  }

  def parser(_input: String) = new MaraParser[Eval, EvalLiteral with EvalOperator with EvalControlFlow with EvalVariable] {
    val alg = new C with EvalLiteral with EvalOperator with EvalControlFlow with EvalVariable
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
  println(parse("0&&1"))
  println(parse("0~&1"))
  println(parse("0@1$2%3^4^5%6$7@8"))
  println(parse("valx:Int"))
  println(parse("valx:Int=10"))
  println(parse("x"))
  println(parse("do{valx:Int=20;x}"))
  println(parse("do{valx=30;x}"))
}
