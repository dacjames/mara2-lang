package io.dac.mara

import io.dac.mara.core.Record
import monix.reactive.{Observable, Observer}
import monix.execution.Ack
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future


/**
  * Created by dcollins on 8/2/16.
  */
object CLI extends MaraLanguage with App {
  def debug(text: String) = s"${show(text)} => ${eval(text)}"

  val observable = Observable(1, 2, 3)
  val observer = new Observer[Int] {
    override def onNext(elem: Int): Future[Ack] = {
      println(elem)
      Ack.Continue
    }

    override def onError(ex: Throwable): Unit = {
      println(s"Error: ${ex}")
    }

    override def onComplete(): Unit = {
      println("Done!")
    }
  }

  observable.subscribe(observer)

  val r = Record(1 -> "hello", 2 -> "world")
  val q = Record(1 -> "hello", "qua" -> "wex")
  println(s"R: ${r} ${r.get(1)}, Q: ${q}, ${q.get("qua")}, ${q(1)}")


//  println(debug("3^( 7*4 ) <= 3 ^ 7*4||1&&1<=1"))
//  println(debug("3+~1+1"))
//  println(debug("if1{3}"))
//  println(debug("if 0 { 3 } else{4}"))
//  println(debug("0&&1"))
//  println(debug("0~&1"))
//  println(debug("0@1$2%3^4^5%6$7@8"))
//  println(debug("valx:Int"))
//  println(debug("valx:Int=10"))
//  println(debug("x"))
//  println(debug("do{valx:Int=20;x}"))
//  println(debug("do{valx=30;x}"))
//  println(debug("___T"))
//
}
