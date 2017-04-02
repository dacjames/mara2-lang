package io.dac.mara.lang.compound

import io.dac.mara.core.MaraValue.EmptyValue
import io.dac.mara.exprops.{Eval, EvalOp}
import io.dac.mara.core.{MaraValue, Namespace, Record}

import scala.collection.mutable

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
          Record.construct[MaraValue](elems: _*) match {
            case Left(s) => ErrorValue(s"Cannot construct Record: ${s}")
            case Right(r) => RecordValue(r)
          }
        case IntValue(s) =>
          val elems = tags.map {
            case (k, v) =>
              k.eval match {
                case IntValue(i) => i -> v.eval
              }
          }
          Record.construct[MaraValue](elems: _*) match {
            case Left(s) => ErrorValue(s"Cannot construct Record: ${s}")
            case Right(r) => RecordValue(r)
          }
      }
    } catch {
      case e: MatchError => ErrorValue("Cannot mix string and integer keys when constructing a record literal")
    }
  }

  override def get(name: String, args: Seq[Eval]): Eval = op {
    val record = lookupValue(name)

    val newIt = record match {
      case MaraValue.RecordValue(r: Record[MaraValue]) =>
        val keys = args.map(_.eval)

        keys.length match {
          case 0 => ErrorValue("Should be impossible")
          case 1 => keys(0) match {
            case StringValue(s) => r.get(s).getOrElse(ErrorValue(s"Key ${s} not found in Record ${r}"))
            case IntValue(i) => r.get(i).getOrElse(ErrorValue(s"Pos ${i} not found in Record ${i}"))
          }
          case _ =>
            val tags: Seq[(Int, MaraValue)] = keys.zipWithIndex.map {
              case (IntValue(i), p) => (p, r.get(i).getOrElse(ErrorValue(s"Pos ${i} not found in Record ${i}")))
              case (StringValue(s), p) => (p, r.get(s).getOrElse(ErrorValue(s"Key ${s} not found in Record ${s}")))
            }

            RecordValue(Record[MaraValue](tags: _*))
        }

      case _ => ErrorValue(s"Get only applys to records, not ${record}")
    }

//    record match {
//      case MaraValue.RecordValue(r) => {
//        val keys = args.map(_.eval)
//
//        keys.length match {
//          case 0 => ErrorValue("Should be impossible")
//          case 1 => ( keys(0) match {
//            case StringValue(s) => r.get(s).getOrElse(ErrorValue(s"Key ${s} not found in Record ${r}"))
//            case IntValue(i) => r.get(i).getOrElse(ErrorValue(s"Pos ${i} not found in Record ${i}"))
//          } ).asInstanceOf[MaraValue]
//          case _ => {
//            val byKey = keys.collect{
//              case StringValue(s) => (s, r(s).asInstanceOf[MaraValue])
//            }
//
//            val byPos: Seq[(Int, MaraValue)] = keys.zipWithIndex.collect {
//              case (IntValue(i), p) => (p, r.get(i).get.asInstanceOf[MaraValue])
//            }
//
//            (byKey.length, byPos.length) match {
//              case (0, _) => RecordValue(Record[MaraValue](byPos: _*))
//              case (_, 0) => RecordValue(Record[MaraValue](byKey: _*))
//              case (_, _) => ???
//            }
//
//          }
//        }
//
//      }
//      case _ => ErrorValue(s"Get only applys to records, not ${record}")
//    }

    newIt

  }
}
