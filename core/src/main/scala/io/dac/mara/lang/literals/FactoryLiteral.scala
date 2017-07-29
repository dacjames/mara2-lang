package io.dac.mara.lang.literals

import io.dac.mara.exprops.{Factory, FactoryOp, Node}
import io.dac.mara.lang.root.LangAlg

trait FactoryLiteral extends FactoryOp with LiteralAlg[Factory] {

  case class LitInt(it: Int) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: LiteralAlg[E] => alg.litint(it)
    }
  }

  case class LitString(it: String) extends Node {
    def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: LiteralAlg[E] => alg.litstring(it)
    }
  }

  case class LitBool(it: Boolean) extends Node {
    def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: LiteralAlg[E] => alg.litbool(it)
    }
  }

  override def litint(it: Int): Factory = op { LitInt(it) }

  override def litstring(it: String): Factory = op { LitString(it) }

  override def litbool(it: Boolean): Factory = op { LitBool(it) }
}
