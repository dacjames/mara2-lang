package io.dac.mara.lang.compound

import io.dac.mara.exprops.{Typed, TypedOp}
import io.dac.mara.core.MaraType
import io.dac.mara.core.Record

/**
  * Created by dcollins on 3/24/17.
  */
trait TypedCompound extends TypedOp with CompoundAlg[Typed] {
  import MaraType._
  override def empty = op { EmptyType() }

  private[this] val stripEmpty =
    new PartialFunction[Typed, MaraType] {
      private[this] var it: MaraType = _
      override def isDefinedAt(x: Typed) = { it = x.typex; ! it.isInstanceOf[EmptyType]}
      override def apply(v1: Typed) = it
    }

  override def dox(exprs: Seq[Typed]): Typed = op {
    exprs.collect(stripEmpty).last
  }

  override def list(exprs: Seq[Typed]): Typed = op {
    import Record._

    val tags = exprs.collect(stripEmpty).zipWithIndex map {
      case (t, i) => (IntKey(i), MaraType.promote(t))
    }

    Record.construct(tags: _*) match {
      case Left(msg) => ErrorType(msg)
      case Right(r) => RecordType(r)
    }
  }

  override def record(tags: Seq[(Typed, Typed)]) = op {
    val kvps = tags.map {
      case (k, v) =>
        k.typex match {
          case s: StringLiteralType => TagType(s, MaraType.promote(v.typex))
          case i: IntLiteralType => TagType(i, MaraType.promote(v.typex))
        }
    }

    import Record._

    val newKvps = tags.map {
      case (k, v) =>
        k.typex match {
          case StringLiteralType(s) => (StringKey(s), MaraType.promote(v.typex))
          case IntLiteralType(i) => (IntKey(i), MaraType.promote(v.typex))
        }
    }

    Record.construct[MaraType](newKvps: _*) match {
      case Left(msg) => ErrorType(msg)
      case Right(r) => RecordType(r)
    }
  }


}
