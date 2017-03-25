package io.dac.macros

/**
  * Created by dcollins on 3/25/17.
  */
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object PipelineBuilder {
  def myassert(cond: Boolean, msg: Any) = macro assertImpl

  def assertImpl(c: Context)(cond: c.Expr[Boolean], msg: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._

    val tree = q"if ($cond) { } else { throw new AssertionError($msg) }"
    c.Expr[Unit](tree)
  }

}
