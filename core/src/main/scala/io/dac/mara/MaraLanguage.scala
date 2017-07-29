package io.dac.mara

import java.io.{PrintWriter, StringWriter}

import io.dac.mara.core.{Expr, PhaseContext}
import io.dac.mara.core.Expr.Replable
import io.dac.mara.utils.TimeIt
import io.dac.mara.exprops._
import io.dac.mara.lang.literals.PipelineLiteral
import io.dac.mara.lang.root.{LangAlg, LangParser}
import org.parboiled2.{ErrorFormatter, ParseError, ParserInput}

import scala.util.{Failure, Success, Try}

/**
  * Created by dcollins on 8/20/16.
  */
trait MaraLanguage extends TimeIt {

  import Show._
  import Eval._
  import Typed._


  private[this] class C

  private[this] def trace2string(e: Throwable) = {
    val sw = new StringWriter
    e.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  implicit val context = new PhaseContext

  private[this] def run[E, Alg <: LangAlg[E], R](parser: LangParser[E, Alg])(implicit f: Expr.Replable[E, R]) =
    timeInner("Parser") {
      parser.Root.run()
    } match {
      case Success(result) => timeInner("Substitution") {
        f.value(result).toString
      }
      case Failure(error: ParseError) => parser.formatError(error, new ErrorFormatter(showTraces = true))
      case Failure(error: Throwable) => trace2string(error)
    }

  private def showParser(text: String) =
    new MaraParser[Show, lang.CombinedShow] {
      val input = ParserInput(text)
      val alg = lang.alg.show
    }

  private  def evalParser(text: String) =
    new MaraParser[Eval, lang.CombinedEval] {
      val input = ParserInput(text)
      val alg = lang.alg.eval
    }

  private def typedParser(text: String) =
    new MaraParser[Typed, lang.CombinedTyped] {
      val input = ParserInput(text)
      val alg = lang.alg.typed
    }

  private def compiledParser(text: String) =
    new MaraParser[Compiled, lang.CombinedCompiled] {
      val input = ParserInput(text)
      val alg = lang.alg.compiled
    }

  def show(text: String) = run {
    showParser(text)
  }

  def eval(text: String) = run {
    evalParser(text)
  }


  def typed(text: String) = run {
    typedParser(text)
  }

//  def compiled(text: String) = run {
//    compiledParser(text)
//  }

  def pipeline[E <: Expr[E], R](text: String) = {
    var parser = showParser(text)

    (for {
      showResult <- showParser(text).Root.run()
      evalResult <- evalParser(text).Root.run()
      typedResult <- typedParser(text).Root.run()
    } yield {
      s"${showResult.show} :: ${typedResult.typex} ==> ${evalResult.eval}"
    }) match {
      case Success(it) => it
      case Failure(error: ParseError) => parser.formatError(error, new ErrorFormatter(showTraces = true))
      case Failure(error: Throwable) => trace2string(error)
    }
  }
//
//  def litpipeline(text: String) = {
//
//    implicit object pipelineReplable$ extends Replable[(Show, Typed, Eval), String] {
//      override def value(e: (Show, Typed, Eval)) = s"${e._1.show} :: ${e._2.typex} ==> ${e._3.eval}"
//    }
//
//    val parser = new MaraParser[(Show, Typed, Eval), lang.CombinedAlg[(Show, Typed, Eval)]] {
//      val alg = new PipelineLiteral(lang.alg.show, lang.alg.typed, lang.alg.eval).asInstanceOf[lang.CombinedAlg[(Show, Typed, Eval)]  ]
//      val input = ParserInput(text)
//    }
//
//    parser.Root.run() map {
//      case (s: Show, t: Typed, e: Eval) => s"${s.show} :: ${t.typex} ==> ${e.eval}"
//    }
//  }


}
