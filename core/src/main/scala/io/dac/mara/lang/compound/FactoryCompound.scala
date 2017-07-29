package io.dac.mara.lang.compound

import io.dac.mara.exprops.{Factory, FactoryOp, Node}
import io.dac.mara.lang.root.LangAlg

trait FactoryCompound extends FactoryOp with CompoundAlg[Factory] {
  case object Empty extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: CompoundAlg[E] => alg.empty
    }
  }

  case class Do(block: Seq[Factory]) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: CompoundAlg[E] => alg.dox(block.map(_.build.exec(alg)))
    }
  }

  case class List(exprs: Seq[Factory]) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: CompoundAlg[E] => alg.list(exprs.map(_.build.exec(alg)))
    }
  }

  case class Record(tags: Seq[(Factory, Factory)]) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: CompoundAlg[E] => alg.record(tags map {
        case (k, v) => (k.build.exec(alg), v.build.exec(alg))
      })
    }
  }

  case class Get(name: String, args: Seq[Factory]) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: CompoundAlg[E] => alg.get(name, args.map(_.build.exec(alg)))
    }
  }

  override def empty: Factory = op { Empty }

  override def dox(block: Seq[Factory]): Factory = op { Do(block) }

  override def list(exprs: Seq[Factory]): Factory = op { List(exprs) }

  override def record(tags: Seq[(Factory, Factory)]): Factory = op { Record(tags) }

  override def get(name: String, args: Seq[Factory]): Factory = op { Get(name, args) }
}
