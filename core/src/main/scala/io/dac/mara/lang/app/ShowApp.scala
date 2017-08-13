package io.dac.mara.lang.app

import io.dac.mara.core._
import io.dac.mara.phases.{Show, ShowOp}

trait ShowApp extends ShowOp with AppAlg[Show] {
  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Show]): Show = op {
    s"app ${name}(${args.map(_.toString).mkString(", ")}) {" ++
      (if (body.isEmpty) " }"
      else "\n" ++ body.map(_.show).mkString("\n") ++ s"}")
  }
}
