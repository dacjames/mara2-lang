package io.dac.mara.lang.functions

import io.dac.mara.lang.parsers._
import io.dac.mara.core._
import org.parboiled2._

/**
  * Created by dcollins on 8/28/16.
  */
trait FunctionParser[E, Alg <: FunctionAlg[E]] extends LangParser[E, Alg]
  with IdentifierParser with BlockParser[E, Alg] with TupleParser[E, Alg] with PairParser {

  def Function: Rule1[E] = rule {
    ConcreteDef | AbstractDef
  }

  def Call: Rule1[E] = rule {
    "." ~ ValueId ~ optional(TupleSyntax) ~> { (a: String, b: Option[Seq[E]]) =>
      b match {
        case Some(b) => alg.call(a, b)
        case None => alg.call(a, Seq.empty[E])
      }
    }
  }

  private[this] def AbstractDef: Rule1[E] = rule {
    "def" ~ ValueId ~ Pairs ~ optional("->" ~ TypeId) ~> {
      (a: String, b: (Seq[Pair.Type], Seq[Pair.Value]), c: Option[String]) => {
        val typeparams = b._1
        val valparams = b._2
        alg.defabstract(name = a, typeparams = typeparams, valparams = valparams, typex = c)
      }
    }
  }

  private[this] def ConcreteDef: Rule1[E] = rule {
    "def" ~ ValueId ~ Pairs ~ optional("->" ~ TypeId) ~ Block ~> {
      (a: String, b: (Seq[Pair.Type], Seq[Pair.Value]), c: Option[String], d: Seq[E] ) => {
        val typeparams = b._1
        val valparams = b._2
        alg.defconcrete(name = a, typeparams = typeparams, valparams = valparams, typex = c, body = d)
      }
    }
  }


}
