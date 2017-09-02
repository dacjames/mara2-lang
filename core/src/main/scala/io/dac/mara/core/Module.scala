package io.dac.mara.core

import io.dac.mara.ir.IrModel.{Fragment, Instruction}

import scala.collection.mutable

class Module {
  private[core] val symbols: mutable.HashMap[String, Fragment] = mutable.HashMap.empty
  private[core] var counter: Int = 0

  def clear(): Unit = {
    symbols.clear()
    counter = 0
  }
}

trait ModuleLookup {
  def module: Module

  def addSymbol(name: String, code: Fragment): Unit =
    module.symbols.put(name, code)

  def getModuleCode: Fragment =
    Fragment(module.symbols.values.flatten.toSeq)

  def nextTemp(): String = {
    val temp = s"%t${module.counter}"
    module.counter += 1
    temp
  }
}

