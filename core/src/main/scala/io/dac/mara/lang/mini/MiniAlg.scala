package io.dac.mara.lang.mini


import java.util.concurrent.{ConcurrentHashMap, Executor}

import io.dac.mara.core.MaraValue.{ErrorValue, IntValue}
import io.dac.mara.core.{MaraType, MaraValue}
import io.dac.mara.lang.root.LangAlg
import io.dac.mara.utils.TimeIt

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.collection.concurrent
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


case class Phase(index: Int) extends AnyVal
abstract class PhaseKey[A] {
  def phaseIndex: Int
}

abstract class MiniExpr[E <: MiniExpr[E]](val phase: Phase, context: PhaseContext){
  type Target

  def exec: Future[E#Target]

  def await(atMost: Duration): E#Target =
    Await.result(this.exec, atMost)

  def retrieve[E <: MiniExpr[_]: PhaseKey]: Future[E#Target] =
    context.retrieve[E](this.phase)
}

abstract class ExprOps[E <: MiniExpr[E]: PhaseKey] {
  implicit val phaseContext: PhaseContext


  def build(fn: Phase => Future[E#Target])(phase: Phase): E

  def op(f: Phase => Future[E#Target]): E = {
    phaseContext.track[E](build(f))
  }
}

// ################## Expr Types ################## //

abstract class MiniShow(phase: Phase)
                       (implicit context: PhaseContext)
  extends MiniExpr[MiniShow](phase, context) {
  override type Target = String
}

object MiniShow {
  implicit object MiniShowKey extends PhaseKey[MiniShow] {
    override def phaseIndex: Int = 0
  }
}

abstract class MiniShowOp extends ExprOps[MiniShow] {
  override def build(fn: (Phase) => Future[String])
                    (phase: Phase): MiniShow =
    new MiniShow(phase) {
      override def exec = fn(this.phase)
    }
}

abstract class MiniEval(phase: Phase)
                       (implicit context: PhaseContext)
  extends MiniExpr[MiniEval](phase, context) {
  override type Target = MaraValue
}

object MiniEval {
  implicit object MiniEvalKey extends PhaseKey[MiniEval] {
    override def phaseIndex: Int = 1
  }
}

abstract class MiniEvalOp extends ExprOps[MiniEval] {
  override def build(fn: (Phase) => Future[MaraValue])
                    (phase: Phase): MiniEval =
    new MiniEval(phase) {
      override def exec = fn(this.phase)
    }
}



class PhaseContext {

  implicit def wrapPhase: Int => Phase = Phase
  implicit def unwrapPhase: Phase => Int = _.index


  private[this] val exprLogs: mutable.ArrayBuffer[mutable.ArrayBuffer[Any]] =
    mutable.ArrayBuffer.empty

  def phase[E: PhaseKey]: Phase =
    Phase(exprLogs(implicitly[PhaseKey[E]].phaseIndex).size - 1)

  def track[E <: MiniExpr[_]: PhaseKey](node: Phase => E): E = {
    val index = implicitly[PhaseKey[E]].phaseIndex
    while (index >= exprLogs.size) {
      exprLogs += mutable.ArrayBuffer.empty
    }

    val targetLog = exprLogs(index)
    val thunk = node(targetLog.size)
    targetLog += thunk

    thunk
  }

  private[this] def coerce[E <: MiniExpr[E]](expr: Any): Future[E#Target] =
    expr.asInstanceOf[E].exec


  def retrieve[E <: MiniExpr[_]: PhaseKey](phase: Phase): Future[E#Target] = {
    val index = implicitly[PhaseKey[E]].phaseIndex
    val result =
      if (index < exprLogs.size && phase.index < exprLogs(index).size) {
        coerce(exprLogs(index)(phase.index))
      } else {
        Future.failed(new Exception("Unknown phase error!"))
      }

    result
  }

}

// ################## Algebras ################## //

trait MiniAlg[E] extends LangAlg[E] {
  def litint(i: Int): E
  def add(x: E, y: E): E
}

trait MiniAlg2[E] extends LangAlg[E] {
  def sub(x: E, y: E): E
}


trait EvalMini extends MiniEvalOp with MiniAlg[MiniEval] with MiniAlg2[MiniEval] {
  import io.dac.mara.core.MaraValue._

  override def litint(i: Int): MiniEval = op { phase =>
    for {
      s <- phaseContext.retrieve[MiniShow](phase)
    } yield {
      val r = IntValue(i)
      println(s"Eval:${phase}: ${r}")
      r
    }
  }

  override def add(x: MiniEval, y: MiniEval): MiniEval = op { phase =>
    for {
      ax <- x.exec
      ay <- y.exec
      sx <- x.retrieve[MiniShow]
      sy <- y.retrieve[MiniShow]
    } yield (ax, ay) match {
      case (IntValue(a), IntValue(b)) =>
        val r = IntValue(a + b)
        println(s"Eval:${phase}: ${sx} (+) ${sy} = ${r} ")
        r
      case _ =>
        ErrorValue("???")
    }
  }

  override def sub(x: MiniEval, y: MiniEval): MiniEval = op { phase =>
    for {
      ax <- x.exec
      ay <- y.exec
    } yield (ax, ay) match {
      case (IntValue(a), IntValue(b)) =>
        IntValue(a - b)
      case _ =>
        ErrorValue("???")
    }
  }
}


trait ShowMini extends MiniShowOp with MiniAlg[MiniShow] {
  override def litint(i: Int): MiniShow = op { phase =>
    Future {
      val r = i.toString
      r
    }
  }

  override def add(x: MiniShow, y: MiniShow): MiniShow = op { phase =>
    for {
      a <- y.exec
      b <- x.exec
    } yield {
      val r = s"${b} + ${a}"
      r
    }
  }
}

trait ShowMini2 extends MiniShowOp with MiniAlg2[MiniShow] {
  override def sub(x: MiniShow, y: MiniShow): MiniShow = op { phase =>
    for {
      a <- y.exec
      b <- x.exec
    } yield {
      val r = s"${b} - ${a}"
      r
    }
  }
}


case class PipelineResult(implicit val context: PhaseContext) {
  def retrieve[E <: MiniExpr[E]: PhaseKey](implicit cls: ClassTag[E]): Future[E#Target] =
    context.retrieve[E](context.phase[E])

  def await[E <: MiniExpr[E]: PhaseKey](atMost: Duration)(implicit cls: ClassTag[E]) =
    Await.result(retrieve[E], atMost)
}

class MiniLang {
  implicit val langContextTracker = new PhaseContext


  type LangAlg[E] = MiniAlg[E] with MiniAlg2[E]

  private[this] def stages: Seq[LangAlg[MiniExpr[_]]] = Seq(

    new EvalMini {
      override val phaseContext = langContextTracker
    }.asInstanceOf[LangAlg[MiniExpr[_]]],

    new ShowMini with ShowMini2 {
      override val phaseContext = langContextTracker
    }.asInstanceOf[LangAlg[MiniExpr[_]]]

  )

  def pipeline(expr: LangAlg[MiniExpr[_]] => MiniExpr[_]): PipelineResult = {
    stages.map(expr).foreach(_.exec)
    PipelineResult()
  }

  def parPipeline(expr: LangAlg[MiniExpr[_]] => MiniExpr[_]): PipelineResult = {
    stages.par.map(expr).foreach(_.exec)
    PipelineResult()
  }

}


object MiniTest extends App with TimeIt {
  val lang = new MiniLang



  def expr(alg: MiniLang#LangAlg[MiniExpr[_]]): MiniExpr[_] =
    alg.sub(alg.add(alg.add(alg.litint(1), alg.litint(2)), alg.litint(3)), alg.litint(3))

  timeIt {
    val result = lang.pipeline(expr)
    println(s"${result.await[MiniShow](1.second)} ==> ${result.await[MiniEval](1.second)}")
  }

  timeIt {
    val result =
      lang.pipeline(expr)

    println(s"${result.await[MiniShow](1.second)} ==> ${result.await[MiniEval](1.second)}")
  }
//
//
//    println(s"${result.await[MiniShow](1.second)} ==> ${result.await[MiniEval](1.second)}")
//  }



}
