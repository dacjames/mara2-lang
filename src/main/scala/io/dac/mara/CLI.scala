package io.dac.mara

import org.parboiled2._

import scala.util.{Failure, Success}


class C

/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends App {
  println("Hello, World!")

  def parser(_input: String) = new MaraParser[Eval, EvalLiteral with EvalArithmetic] {
    val alg = new C with EvalLiteral with EvalArithmetic
    val input = ParserInput(_input)
  }

  def parse(input: String) = {
    val p = parser(input)
    p.InputLine.run() match {
      case Success(result) => result.eval.toString
      case Failure(error: ParseError) => p.formatError(error)
    }
  }

  println(parse("3^(7*4)"))

}
