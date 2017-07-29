package io.dac.mara.lang.literals
//
//import io.dac.mara.exprops.{Node, Tree, TreeOp}
//
///**
//  * Created by dcollins on 8/20/16.
//  */
//trait TreeLiteral extends TreeOp with LiteralAlg[Tree]{
//  override def litint(it: Int) = op { Node("litint", it) }
//
//  override def litstring(it: String) = op { Node("litstring", it) }
//}