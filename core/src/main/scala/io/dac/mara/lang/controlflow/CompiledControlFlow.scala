package io.dac.mara.lang.controlflow

import io.dac.mara.core.{ModuleLookup, NamespaceLookup}
import io.dac.mara.phases.{Compiled, CompiledOp}

trait CompiledControlFlow extends CompiledOp with ControlFlowAlg[Compiled] with NamespaceLookup with ModuleLookup {
  import io.dac.mara.ir.IrModel._

  override def ifx(pred: Compiled, body: Compiled) = op {
    val predCode = pred.fragment

    val bodyCode =
      label("body", "body:") ++
      body.fragment

    val otherCode =
      label("otherwise", "otherwise:")

    (predCode :+
      stmt(s"br i1 ${predCode.result}, label ${bodyCode.block}, label ${otherCode.block}")) ++
      bodyCode ++
      otherCode
  }
}
