package io.dac.mara

import java.io.{PrintWriter, StringWriter}

import io.dac.mara.core.{Expr, TreeContext, _}
import io.dac.mara.phases._
import io.dac.mara.utils.TimeIt
import org.parboiled2.{ErrorFormatter, ParseError, ParserInput}

import scala.collection.mutable
import scala.util.{Failure, Success}

/**
  * Created by dcollins on 8/20/16.
  */
trait MaraLanguage extends TimeIt {

  private[this] def trace2string(e: Throwable) = {
    val sw = new StringWriter
    e.printStackTrace(new PrintWriter(sw))
    sw.toString
  }

  implicit val context = new TreeContext

  private def factoryParser(text: String) =
    new MaraParser[Factory, lang.CombinedFactory] {
      val input = ParserInput(text)
      val alg = lang.alg.factory
    }

  def fresh[A](block: => A) = {
    lang.clear()
    context.clear()
    block
  }

  def run[E <: Expr[E]: Empty, R](text: String, alg: ExprAlg[E]) = {
    val parser = factoryParser(text)
    parser.Root.run().map { factory =>
      val tree = factory.build
      tree.exec(alg)
    } match {
      case Success(result) => result.value.toString
      case Failure(error: ParseError) => parser.formatError(error, new ErrorFormatter(showTraces = true))
      case Failure(error: Throwable) => trace2string(error)
    }
  }

  def runFactory[E <: Expr[E], R](text: String) = {
    val parser = factoryParser(text)
    parser.Root.run() match {
      case Success(result) => result.value.toString
      case Failure(error: ParseError) => parser.formatError(error, new ErrorFormatter(showTraces = true))
      case Failure(error: Throwable) => trace2string(error)
    }
  }

  def pipeline[E <: Expr[E], R](text: String) = {
    var parser = factoryParser(text)
    timeInner("Parser")(parser.Root.run()) map { factory =>
      val tree = factory.build
      val showResult = timeInner("Show")(tree.exec(lang.alg.show))
      val typedResult = timeInner("Typed")(tree.exec(lang.alg.typed))
      val evalResult = timeInner("Eval")(tree.exec(lang.alg.eval))

      s"${showResult.show} :: ${typedResult.typex} ==> ${evalResult.eval}"
    } match {
      case Success(it) => it
      case Failure(error: ParseError) => parser.formatError(error, new ErrorFormatter(showTraces = true))
      case Failure(error: Throwable) => trace2string(error)
    }
  }

  def fullPipeline[E <: Expr[E]: Phase](text: String) = {
    val n = implicitly[Phase[E]].key
    val results = mutable.HashMap.empty[Int, String]

    var parser = factoryParser(text)
    timeInner("Parser")(parser.Root.run()) map { factory =>
      val tree = factory.build
      val showResult = timeInner("Show")(tree.exec(lang.alg.show)).value
      val typedResult = timeInner("Typed")(tree.exec(lang.alg.typed)).value
      val compiledResult = timeInner("Compiled")(tree.exec(lang.alg.compiled)).value
      val stagedResult = timeInner("Staged")(tree.exec(lang.alg.staged)).value
      val evalResult = timeInner("Eval")(tree.exec(lang.alg.eval)).value

      results.put(implicitly[Phase[Show]].key, showResult.toString)
      results.put(implicitly[Phase[Typed]].key, typedResult.toString)
      results.put(implicitly[Phase[Compiled]].key, compiledResult.toString)
      results.put(implicitly[Phase[Staged]].key, stagedResult.toString)
      results.put(implicitly[Phase[Eval]].key, evalResult.toString)

      results(n)
    } match {
      case Success(it) => it
      case Failure(error: ParseError) => parser.formatError(error, new ErrorFormatter(showTraces = true))
      case Failure(error: Throwable) => trace2string(error)
    }
  }


  def show(text: String) = run(text, lang.alg.show)
  def eval(text: String) = run(text, lang.alg.eval)
  def typed(text: String) = run(text, lang.alg.typed)
  def compiled(text: String) = run(text, lang.alg.compiled)
  def factory(text: String) = runFactory(text)

}
