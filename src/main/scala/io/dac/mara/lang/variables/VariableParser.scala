package io.dac.mara.lang.variables

import io.dac.mara.core.Expr
import io.dac.mara.lang.parsers.IdentifierParser
import io.dac.mara.lang.root.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 8/12/16.
  */
trait VariableParser [E <: Expr, Alg <: VariableAlg[E]] extends LangParser[E, Alg] with IdentifierParser {

  def Variable: Rule1[E] = rule {
    Assign | Declare
  }

  def Substitution: Rule1[E] = rule {
    ValueId ~> {  (x: String) => alg.valsubstitution(x) } |
    TypeId ~> { (x: String) => alg.typesubstitution(x) }
  }

  private def Declare = rule {
    "val" ~ ValueId ~ ":" ~ TypeId ~> { (x: String, y: String) => alg.valdeclare(x, Some(y))} |
    "val" ~ ValueId ~> { (x: String) => alg.valdeclare(x, None) }
  }

  private def Assign = rule {
    "val" ~ ValueId ~ ":" ~ TypeId ~ "=" ~ Expr ~> {
      (x: String, y: String, z: E) =>
        alg.valassign(x, Some(y), z)
    } |
    "val" ~ ValueId ~ "=" ~ Expr ~> {
      (x: String, z: E) =>
        alg.valassign(x, None, z)
    }
  }

}
