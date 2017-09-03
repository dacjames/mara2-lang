package io.dac.mara.core

import io.dac.mara.core.MaraConstant.UnitConstant
import io.dac.mara.ir.IrModel._

import scala.collection.mutable
import scala.reflect.ClassTag

class Module {
  private[core] val symbols: mutable.HashMap[String, Fragment] = mutable.HashMap.empty
  private[core] var counter: Int = 0

  private[this] def constants: Vector[MaraConstant] = Vector(
    UnitConstant
  )

  def constant[A <: MaraConstant: ClassTag]: A = {
    constants.collect {
      case it: A => it
    }.head
  }

  def header: Fragment = {
    constants.map { c =>
      c match {
        case UnitConstant =>
          val ref = l(c.ref)
          stmt(ref, r(s"constant i32* inttoptr (i32 1 to i32*)")) : Fragment
        case _ =>
          Fragment.empty
      }

    }.foldLeft(Fragment.empty)(_ ++ _)
  }

  def nextTemp(): String = {
    val temp = s"%t${this.counter}"
    this.counter += 1
    temp
  }

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
    module.header ++
    Fragment(module.symbols.values.flatten.toSeq)

  def nextTemp(): String =
    module.nextTemp()


}

