package io.dac.mara.lang.compound

import io.dac.mara.core.MaraValue.EmptyValue
import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.core.{MaraValue, Record}

import scala.collection.mutable

/**
  * Created by dcollins on 3/24/17.
  */
trait EvalCompound extends EvalOp with CompoundAlg[Eval] {
  import MaraValue._

  override def empty = op { EmptyValue() }

  private[this] val stripEmpty =
    new PartialFunction[Eval, MaraValue] {
      private[this] var it: MaraValue = _
      override def isDefinedAt(x: Eval) = { it = x.eval; ! it.isInstanceOf[EmptyValue]}
      override def apply(v1: Eval) = it
    }

  override def dox(block: Seq[Eval]) =
    op {
      block.collect(stripEmpty).last
    }

  override def list(exprs: Seq[Eval]): Eval = op {
    val elems = exprs.collect(stripEmpty).zipWithIndex.map {
      case (e, i) => i -> e
    }

    val record = Record[MaraValue](elems: _*)

    RecordValue(record)
  }

  override def record(tags: Seq[(Eval, Eval)]): Eval = op {
    val firstKey = tags.head._1.eval
    try {
      firstKey match {
        case StringValue(i) =>
          val elems = tags.map {
            case (k, v) =>
              k.eval match {
                case StringValue(s) => s -> v.eval
              }
          }
          RecordValue(Record[MaraValue](elems: _*))
        case IntValue(s) =>
          val elems = tags.map {
            case (k, v) =>
              k.eval match {
                case IntValue(i) => i -> v.eval
              }
          }
          RecordValue(Record[MaraValue](elems: _*))
      }
    } catch {
      case e: MatchError => ErrorValue("Cannot mix string and integer keys when constructing a record literal")
    }

  }
}
