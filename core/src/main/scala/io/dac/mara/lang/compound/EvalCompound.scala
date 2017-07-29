package io.dac.mara.lang.compound

import io.dac.mara.core.{MaraValue, Namespace, Record}
import io.dac.mara.phases.{Eval, EvalOp}

/**
  * Created by dcollins on 3/24/17.
  */
trait EvalCompound extends EvalOp with Namespace with CompoundAlg[Eval] {
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
    import Record._

    val kvps = tags.map {
      case (k, v) =>
        k.eval match {
          case StringValue(s) => (StringKey(s), v.eval)
          case IntValue(i) => (IntKey(i), v.eval)
        }
    }

    Record.construct[MaraValue](kvps: _*) match {
      case Right(r) => RecordValue(r)
      case Left(s) => ErrorValue(s)
    }
  }

  override def get(name: String, args: Seq[Eval]): Eval = op {
    val record = lookupValue(name)

    record match {
      case MaraValue.RecordValue(r: Record[MaraValue]) =>
        val keys = args.map(_.eval)


        keys.length match {
          case 0 => ErrorValue("Should be impossible")
          case 1 => keys(0) match {
            case StringValue(s) => r.get(s).getOrElse(ErrorValue(s"Key ${s} not found in Record ${r}"))
            case IntValue(i) => r.get(i).getOrElse(ErrorValue(s"Pos ${i} not found in Record ${i}"))
          }
          case _ =>
            import Record._

            val tags: Seq[(Key, MaraValue)] = keys.zipWithIndex.map {
              case (IntValue(i), p) => (IntKey(p), r.get(i).getOrElse(ErrorValue(s"Pos ${i} not found in Record ${i}")))
              case (StringValue(s), p) => (StringKey(s), r.get(s).getOrElse(ErrorValue(s"Key ${s} not found in Record ${s}")))
            }

            construct(tags: _*) match {
              case Left(s) => ErrorValue(s)
              case Right(r) => RecordValue(r)
            }
        }
      case e: ErrorValue => e
      case _ => ErrorValue(s"Get only applys to records, not ${record}")
    }
  }
}
