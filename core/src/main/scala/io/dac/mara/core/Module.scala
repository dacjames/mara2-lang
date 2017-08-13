package io.dac.mara.core

import io.dac.mara.ir.IrFragment

import scala.collection.mutable

class Module {
  private[core] val symbols: mutable.HashMap[String, Vector[IrFragment]] = mutable.HashMap.empty
  private[core] var counter: Int = 0
}

trait ModuleLookup {
  def module: Module

  def addSymbol(name: String, code: Vector[IrFragment]): Unit =
    module.symbols.put(name, code)

  def getModuleCode: String =
    module.symbols.values.flatten.mkString("", "\n", "\n")

  def nextTemp(): String = {
    val temp = s"%t${module.counter}"
    module.counter += 1
    temp
  }
}

