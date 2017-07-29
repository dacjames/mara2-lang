package io.dac.mara.lang.operators

import io.dac.mara.exprops.{Factory, FactoryOp, Node}
import io.dac.mara.lang.root.LangAlg

trait FactoryOperator extends FactoryOp with OperatorAlg[Factory] {

  case class Plus(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.plus(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Minus(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.minus(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Times(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.times(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Divide(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.divide(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Power(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.power(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Lt(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.lt(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Gt(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.gt(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Lte(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.lte(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Gte(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.gte(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Ne(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.ne(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Eq(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.eq(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class And(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.and(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Or(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.or(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Not(x: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.not(x.build.exec(alg))
    }
  }

  case class Nand(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.nand(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Base(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.base(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Low(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.low(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class Medium(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.medium(x.build.exec(alg), y.build.exec(alg))
    }
  }

  case class High(x: Factory, y: Factory) extends Node {
    override def exec[E](alg: LangAlg[E]): E = alg match {
      case alg: OperatorAlg[E] => alg.high(x.build.exec(alg), y.build.exec(alg))
    }
  }


  override def plus(x: Factory, y: Factory): Factory = op {
    Plus(x, y)
  }

  override def minus(x: Factory, y: Factory): Factory = op {
    Minus(x, y)
  }

  override def times(x: Factory, y: Factory): Factory = op {
    Times(x, y)
  }

  override def divide(x: Factory, y: Factory): Factory = op {
    Divide(x, y)
  }

  override def power(x: Factory, y: Factory): Factory = op {
    Power(x, y)
  }

  override def lt(x: Factory, y: Factory): Factory = op {
    Lt(x, y)
  }

  override def gt(x: Factory, y: Factory): Factory = op {
    Gt(x, y)
  }

  override def lte(x: Factory, y: Factory): Factory = op {
    Lte(x, y)
  }

  override def gte(x: Factory, y: Factory): Factory = op {
    Gte(x, y)
  }

  override def ne(x: Factory, y: Factory): Factory = op {
    Ne(x, y)
  }

  override def eq(x: Factory, y: Factory): Factory = op {
    Eq(x, y)
  }

  override def and(x: Factory, y: Factory): Factory = op {
    And(x, y)
  }

  override def or(x: Factory, y: Factory): Factory = op {
    Or(x, y)
  }

  override def not(x: Factory) = op {
    Not(x)
  }

  override def nand(x: Factory, y: Factory): Factory = op {
    Nand(x, y)
  }

  override def base(x: Factory, y: Factory): Factory = op {
    Base(x, y)
  }

  override def low(x: Factory, y: Factory): Factory = op {
    Low(x, y)
  }

  override def medium(x: Factory, y: Factory): Factory = op {
    Medium(x, y)
  }

  override def high(x: Factory, y: Factory): Factory = op {
    High(x, y)
  }
}
