package io.dac.mara.lang.mini

import java.lang.annotation.Target

import io.dac.mara.core.{MaraValue, Record}
import io.dac.mara.core.MaraValue.IntValue
import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.lang.root.LangAlg

import scala.collection.mutable
import scala.reflect.ClassTag

//trait LangNode[-Alg[_]] {
//  def exec[E]: Alg[E] => E
//}
//
//trait LangNodeOp {
//  def phaseContext: PhaseContext
//
//  def op[E, Alg[+E]](f: => Alg[E] => E) = {
//    phaseContext.track(classOf[LangNode[Alg]]) {
//      new LangNode[Alg] {
//        override def exec[A]: (Alg[E]) => E = f
//      }
//    }
//  }
//}

//abstract class MiniExpr[Result](val phase: Phase, val context: PhaseContext) {
//  def exec: Result
//  def retrieve[A <: MiniExpr[_]](implicit cls: ClassTag[A]): A =
//    context.retrieve[A](this.phase)
//}

abstract class MiniExpr(val phase: Phase, context: PhaseContext){
  type Target

  def exec: Target

  def retrieve[A <: MiniExpr](implicit cls: ClassTag[A]): A#Target =
    context.retrieve[A](this.phase).exec
}


abstract class ExprOps[E <: MiniExpr](implicit cls: ClassTag[E]) {
  implicit val phaseContext: PhaseContext

  def build(fn: Phase => E#Target)(phase: Phase): E

  def op(f: Phase => E#Target) = {
    phaseContext.track[E](build(f))
  }
}

abstract class MiniEval(phase: Phase)
                       (implicit context: PhaseContext)
  extends MiniExpr(phase, context) {
  override type Target = MaraValue
}

abstract class MiniShow(phase: Phase)
                       (implicit context: PhaseContext)
  extends MiniExpr(phase, context) {
  override type Target = String
}

abstract class MiniShowOp extends ExprOps[MiniShow] {
  override def build(fn: (Phase) => String)
                    (phase: Phase): MiniShow =
    new MiniShow(phase) {
      override def exec = fn(this.phase)
    }
}

abstract class MiniEvalOp extends ExprOps[MiniEval] {
  override def build(fn: (Phase) => MaraValue)
                    (phase: Phase): MiniEval =
    new MiniEval(phase) {
      override def exec = fn(this.phase)
    }
}

case class Phase(index: Int) extends AnyVal

trait PhaseContext {
  def phase[E](implicit cls: ClassTag[E]): Phase

  def track[E](node: Phase => E)
              (implicit cls: ClassTag[E]): E

  def retrieve[A](phase: Phase)
                 (implicit cls: ClassTag[A]): A
}

/**
  * Created by dcollins on 5/6/17.
  */
trait MiniAlg[E] extends LangAlg[E] {
  def litint(i: Int): E
  def add(x: E, y: E): E
}

trait EvalAgain[Alg[_]] {
  def evalAgain(evalAlg: Alg[Eval]): MaraValue
}


class PhaseContextTracker extends PhaseContext {
  implicit def wrapPhase: Int => Phase = Phase(_)
  implicit def unwrapPhase: Phase => Int = _.index

  private[this] val counters = mutable.Map.empty[ClassTag[_], Phase]
  private[this] val lookup = mutable.Map.empty[(Phase, ClassTag[_]), Any]

  override def phase[E](implicit cls: ClassTag[E]): Phase =
    counters.getOrElse(cls, Phase(0))

  override def track[E](node: Phase => E)
                       (implicit cls: ClassTag[E]): E = {
    val nextPhase = counters.getOrElse(cls, Phase(0)) + 1
    counters.put(cls, nextPhase)
    val thunk = node(nextPhase)
    lookup.put((nextPhase, cls), thunk)
    thunk
  }

  override def retrieve[A](phase: Phase)
                          (implicit cls: ClassTag[A]): A =
    lookup((phase, cls)).asInstanceOf[A]
}


trait EvalMini extends MiniEvalOp with MiniAlg[MiniEval] {
  import io.dac.mara.core.MaraValue._

  override def litint(i: Int): MiniEval = op { phase =>
    println(s"Show: ${phaseContext.retrieve[MiniShow](phase).exec}")
    IntValue(i)
  }

  override def add(x: MiniEval, y: MiniEval): MiniEval = op { phase =>
    println(s"Show X: ${x.retrieve[MiniShow]}")
    (x.exec, y.exec) match {
      case (IntValue(x), IntValue(y)) =>
        IntValue(x + y)
      case _ =>
        ErrorValue("???")
    }
  }
}

trait ShowMini extends MiniShowOp with MiniAlg[MiniShow] {
  override def litint(i: Int): MiniShow = op { phase =>
    s"litint(${i})"
  }

  override def add(x: MiniShow, y: MiniShow): MiniShow = op { phase =>
    s"add(${x.exec}, ${y.exec})"
  }
}


class MiniLang {
  val langContextTracker = new PhaseContextTracker

  val showAlg = new ShowMini {
    override val phaseContext = langContextTracker
  }
  val evalAlg = new EvalMini {
    override val phaseContext = langContextTracker
  }

  def expr[E](alg: MiniAlg[E]): E =
    alg.add(
      alg.litint(1),
      alg.add(
        alg.litint(2),
        alg.litint(2)
      )
    )

}


object MiniTest extends App {
  val lang = new MiniLang

  println(lang.expr(lang.showAlg).retrieve[MiniShow])
  println(lang.expr(lang.evalAlg).exec)

}
