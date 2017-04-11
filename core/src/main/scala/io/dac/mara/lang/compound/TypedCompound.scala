package io.dac.mara.lang.compound

import io.dac.mara.exprops.{Typed, TypedOp}
import io.dac.mara.core.MaraType

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
    val tags = exprs.collect(stripEmpty).zipWithIndex map {
      case (t, i) => TagType(IntLiteralType(i), MaraType.promote(t))
    }

    RecordType(tags)
  }

  override def record(tags: Seq[(Typed, Typed)]) = op {
    val kvps = tags.map {
      case (k, v) =>
        k.typex match {
          case s: StringLiteralType => TagType(s, MaraType.promote(v.typex))
          case i: IntLiteralType => TagType(i, MaraType.promote(v.typex))
        }
    }
    RecordType(kvps)
  }


}
