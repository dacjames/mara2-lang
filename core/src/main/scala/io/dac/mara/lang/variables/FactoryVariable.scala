package io.dac.mara.lang.variables

import io.dac.mara.exprops.{Factory, FactoryOp, Node}
import io.dac.mara.lang.root.LangAlg

trait FactoryVariable extends FactoryOp with VariableAlg[Factory] {

  case class ValDeclare(name: String, typex: Option[String]) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: VariableAlg[E] => alg.valdeclare(name, typex)
    }
  }
  case class ValAssign(name: String, typex: Option[String], value: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: VariableAlg[E] => alg.valassign(name, typex, value.build.exec(alg))
    }
  }
  case class ValSubstitution(name: String) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: VariableAlg[E] => alg.valsubstitution(name)
    }
  }
  case class TypeSubstitution(name: String) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: VariableAlg[E] => alg.typesubstitution(name)
    }
  }

  override def valdeclare(name: String, typex: Option[String]): Factory = op {
    ValDeclare(name, typex)
  }

  override def valassign(name: String, typex: Option[String], value: Factory): Factory = op {
    ValAssign(name, typex, value)
  }

  override def valsubstitution(name: String): Factory = op {
    ValSubstitution(name)
  }

  override def typesubstitution(name: String): Factory = op {
    TypeSubstitution(name)
  }
}
