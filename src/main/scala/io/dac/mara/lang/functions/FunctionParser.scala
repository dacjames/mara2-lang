package io.dac.mara.lang.functions

import io.dac.mara.core.Expr
import io.dac.mara.lang.parsers.{BlockParser, IdentifierParser}
import io.dac.mara.lang.root.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionParser[E <: Expr, T <: FunctionAlg[E]] extends LangParser[E, T] with IdentifierParser with BlockParser[E] {

  private[this] sealed trait Pair {
    def expr: E
  }
  private[this] case class TypePair(x: String, y: Option[String]) extends Pair {
    def expr = alg.typeparam(x, y)
  }
  private[this] case class ValuePair(x: String, y: Option[String]) extends Pair {
    def expr = alg.valparam(x, y)
  }

  def Function = rule {
    ConcreteDef | AbstractDef
  }

  private[this] def AbstractDef = rule {
    "def" ~ ValueId ~ Params ~ optional("->" ~ TypeId) ~> {
      (a: String, b: (Seq[Pair], Seq[Pair]), c: Option[String]) => {
        val typeparams = b._1.map(_.expr)
        val valparams = b._2.map(_.expr)
        alg.defabstract(name = a, typeparams = typeparams, valparams = valparams, typex = c)
      }
    }
  }

  private[this] def ConcreteDef: Rule1[E] = rule {
    "def" ~ ValueId ~ Params ~ optional("->" ~ TypeId) ~ Block ~> {
      (a: String, b: (Seq[Pair], Seq[Pair]), c: Option[String], d: Seq[E] ) => {
        val typeparams = b._1.map(_.expr)
        val valparams = b._2.map(_.expr)
        alg.defconcrete(name = a, typeparams = typeparams, valparams = valparams, typex = c, body = d)
      }
    }
  }

  private[this] def Params: Rule1[(Seq[Pair], Seq[Pair])] = rule {
    oneOrMore(ValueParamTuple | TypeParamTuple) ~> { (tuples: Seq[Seq[Pair]]) =>
      tuples.flatten.partition {
        _ match {
          case _: TypePair => true
          case _ => false
        }
      }
    }
  }


  private[this] def ValueParamTuple: Rule1[Seq[Pair]] = rule {
    "(" ~ ValueParam ~ zeroOrMore("," ~ ValueParam) ~ ")" ~> {
      (x: Pair, y: Seq[Pair]) => (x +: y)
    }
  }

  private[this] def TypeParamTuple: Rule1[Seq[Pair]] = rule {
    "(" ~ TypeParam ~ zeroOrMore("," ~ TypeParam) ~ ")" ~> {
      (x: Pair, y: Seq[Pair]) => (x +: y)
    }
  }

  private[this] def ValueParam: Rule1[Pair] = rule {
    ValueId ~ ValueType.? ~> { (x: String, y: Option[String]) => ValuePair(x, y) }
  }

  private[this] def TypeParam: Rule1[Pair] = rule {
    TypeId ~ TypeBounds.? ~> { (x: String, y: Option[String]) => TypePair(x, y) }
  }

  private[this] def TypeBounds = rule {
    "::" ~ TypeId ~> { (x: String) => x }
  }

  private[this] def ValueType = rule {
    ":" ~ TypeId ~> { (x: String) => x }
  }

}
