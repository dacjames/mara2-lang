package io.dac.mara

import org.parboiled2._

import scala.util.{Failure, Success}


class C

/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends App {
  println("Hello, World!")

  def parser(_input: String) = new MaraParser[Show, C with ShowLiteral with ShowArithmetic] {
    val alg = new C with ShowLiteral with ShowArithmetic
    val input = ParserInput(_input)
  }

  def parse(input: String) = {
    val p = parser(input)
    p.InputLine.run() match {
      case Success(result) => result.show
      case Failure(error: ParseError) => p.formatError(error)
    }
  }

  println(parse("(3^7*(4))"))

}
