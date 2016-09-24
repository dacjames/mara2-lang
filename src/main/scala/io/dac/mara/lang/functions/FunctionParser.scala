package io.dac.mara.lang.functions

import io.dac.mara.core.Expr
import io.dac.mara.lang.parsers.{BlockParser, IdentifierParser}
import io.dac.mara.lang.root.LangParser
import org.parboiled2._

import scala.collection.{GenTraversable, GenTraversableOnce}

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionParser[E <: Expr, Alg <: FunctionAlg[E]] extends LangParser[E, Alg]
  with IdentifierParser with BlockParser[E, Alg] {

  private[this] sealed trait Pair {
    def expr(alg: Alg): E
  }
  private[this] case class TypePair(x: String, y: Option[String]) extends Pair {
    def expr(alg: Alg) = alg.typeparam(x, y)
  }
  private[this] case class ValuePair(x: String, y: Option[String]) extends Pair {
    def expr(alg: Alg) = alg.valparam(x, y)
  }

  def Function: Rule1[Alg => E] = rule {
    ConcreteDef | AbstractDef
  }

  private[this] def AbstractDef: Rule1[Alg => E] = rule {
    "def" ~ ValueId ~ Params ~ optional("->" ~ TypeId) ~> {
      (a: String, b: (Seq[Pair], Seq[Pair]), c: Option[String]) => (alg: Alg) => {
        val typeparams = b._1.map(_.expr(alg))
        val valparams = b._2.map(_.expr(alg))
        alg.defabstract(name = a, typeparams = typeparams, valparams = valparams, typex = c)
      }
    }
  }

  private[this] def ConcreteDef: Rule1[Alg => E] = rule {
    "def" ~ ValueId ~ Params ~ optional("->" ~ TypeId) ~ Block ~> {
      (a: String, b: (Seq[Pair], Seq[Pair]), c: Option[String], d: Alg => Seq[E] ) => (alg: Alg) => {
        val typeparams = b._1.map(_.expr(alg))
        val valparams = b._2.map(_.expr(alg))
        alg.defconcrete(name = a, typeparams = typeparams, valparams = valparams, typex = c, body = d(alg))
      }
    }
  }

  private[this] def Params: Rule1[(Seq[Pair], Seq[Pair])] = rule {
    oneOrMore(ValueParamTuple | TypeParamTuple) ~> {
      (tuples: Seq[Seq[Pair]]) => {
        tuples.flatten.partition {
          _ match {
            case _: TypePair => true
            case _ => false
          }
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
    ValueId ~ ValueType.? ~> { (x: String, y: Option[String]) =>  ValuePair(x, y) }
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
