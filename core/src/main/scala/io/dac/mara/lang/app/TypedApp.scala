package io.dac.mara.lang.app

import io.dac.mara.core._
import io.dac.mara.phases.{Typed, TypedOp}

trait TypedApp extends TypedOp with AppAlg[Typed] with NamespaceLookup {
  import MaraType._

  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Typed]): Typed = op {
    val output = body.lastOption match {
      case Some(t) => t.typex
      case None => IntLiteralType(0)
    }

    val kvps = args.map {
      case Pair.Value1(name) =>
        name -> AnyType()
      case Pair.Value2(name, qualifier) =>
        name -> lookupType(qualifier)
    }

    val input = RecordType(Record[MaraType](kvps: _*))

    bindType(name, FunctionType(input, output))
  }
}
