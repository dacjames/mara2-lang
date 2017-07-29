package io.dac.mara.lang.functions

import io.dac.mara.lang.parsers.{BlockParser, IdentifierParser, LangParser, TupleParser}
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionParser[E, Alg <: FunctionAlg[E]] extends LangParser[E, Alg]
  with IdentifierParser with BlockParser[E, Alg] with TupleParser[E, Alg] {


  private[this] sealed trait Pair {
    def pair: Alg#Param
  }
  private[this] case class TypePair(x: String, y: Option[String]) extends Pair {
    def pair = (x, y)
  }
  private[this] case class ValuePair(x: String, y: Option[String]) extends Pair {
    def pair = (x, y)
  }

  def Function: Rule1[E] = rule {
    ConcreteDef | AbstractDef
  }

  def Call: Rule1[E] = rule {
    "." ~ ValueId ~ TupleSyntax ~> { (a: String, b: Seq[E]) => {
        alg.call(a, b)
      }
    }
  }


  private[this] def AbstractDef: Rule1[E] = rule {
    "def" ~ ValueId ~ Params ~ optional("->" ~ TypeId) ~> {
      (a: String, b: (Seq[Pair], Seq[Pair]), c: Option[String]) => {
        val typeparams = b._1.map(_.pair)
        val valparams = b._2.map(_.pair)
        alg.defabstract(name = a, typeparams = typeparams, valparams = valparams, typex = c)
      }
    }
  }

  private[this] def ConcreteDef: Rule1[E] = rule {
    "def" ~ ValueId ~ Params ~ optional("->" ~ TypeId) ~ Block ~> {
      (a: String, b: (Seq[Pair], Seq[Pair]), c: Option[String], d: Seq[E] ) => {
        val typeparams = b._1.map(_.pair)
        val valparams = b._2.map(_.pair)
        alg.defconcrete(name = a, typeparams = typeparams, valparams = valparams, typex = c, body = d)
      }
    }
  }

  private[this] def Params: Rule1[(Seq[Pair], Seq[Pair])] = rule {
    oneOrMore(ValueParamTuple | TypeParamTuple) ~> { (tuples: Seq[Seq[Pair]]) =>
      tuples.flatten.partition {
        case _: TypePair => true
        case _ => false
      }
    }
  }


  private[this] def ValueParamTuple: Rule1[Seq[Pair]] = rule {
    "(" ~ ValueParam ~ zeroOrMore("," ~ ValueParam) ~ optional(',') ~ ")" ~> {
      (x: Pair, y: Seq[Pair]) => (x +: y)
    }
  }

  private[this] def TypeParamTuple: Rule1[Seq[Pair]] = rule {
    "(" ~ TypeParam ~ zeroOrMore("," ~ TypeParam) ~ optional(',') ~ ")" ~> {
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
