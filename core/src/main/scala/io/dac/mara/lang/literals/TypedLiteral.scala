package io.dac.mara.lang.literals

import io.dac.mara.exprops.{Typed, TypedOp}
import io.dac.mara.core.MaraType

/**
  * Created by dcollins on 8/27/16.
  */
trait TypedLiteral extends TypedOp with LiteralAlg[Typed] {
  import MaraType._

  override def litbool(it: Boolean): Typed = op { BoolType() }
  override def litint(it: Int): Typed = op { IntType() }
  override def litstring(it: String): Typed = op { StringType() }
}
