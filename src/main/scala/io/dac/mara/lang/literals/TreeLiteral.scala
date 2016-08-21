package io.dac.mara.lang.literals

import io.dac.mara.exprops.{Node, Tree, TreeOp}

/**
  * Created by dcollins on 8/20/16.
  */
trait TreeLiteral extends TreeOp with LiteralAlg[Tree]{
  override def litInt(it: Int) = op { Node("litint", it) }

  override def litString(it: String) = op { Node("litstring", it) }
}