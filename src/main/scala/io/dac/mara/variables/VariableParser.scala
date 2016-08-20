package io.dac.mara.variables

import io.dac.mara.core.{Expr, LangParser}
import io.dac.mara.identifiers.IdentifierParser
import org.parboiled2._

/**
  * Created by dcollins on 8/12/16.
  */
trait VariableParser [E <: Expr, T <: VariableAlg[E]] extends IdentifierParser[E, T] with LangParser[E, T] {

  def Block = rule {
    "do" ~ '{' ~ Expr ~ ';' ~ Expr ~ '}' ~> { (x, y) => alg.block(x, y) }
  }

  def Variable: Rule1[E] = rule {
    Assign | Declare
  }

  def Substitution = rule {
    ValueId ~> { (x: String) => alg.valsubstitution(x) } |
    TypeId ~> { (x: String) => alg.typesubstitution(x) }
  }

  private def Declare = rule {
    "val" ~ ValueId ~ ":" ~ TypeId ~> { (x: String, y: String) => alg.valdeclare(x, Some(y))} |
    "val" ~ ValueId ~> { (x: String) => alg.valdeclare(x, None) }
  }

  private def Assign = rule {
    "val" ~ ValueId ~ ":" ~ TypeId ~ "=" ~ Expr ~> { (x: String, y: String, z: E) => alg.valassign(x, Some(y), z)} |
    "val" ~ ValueId ~ "=" ~ Expr ~> { (x: String, z: E) => alg.valassign(x, None, z)}
  }

}
