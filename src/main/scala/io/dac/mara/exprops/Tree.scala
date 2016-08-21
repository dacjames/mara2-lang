package io.dac.mara.exprops

import io.dac.mara.core.Expr

/**
  * Created by dcollins on 8/20/16.
  */

case class Node(kind: String, values: Any*)

case class Tree(root: Node) extends Expr

object Tree {
  implicit object TreeFamily extends Expr.Family[Tree, Node] {
    override def value(e: Tree) = e.root
  }
}

trait TreeOp {
  def op(f: => Node) = Tree(f)
}
