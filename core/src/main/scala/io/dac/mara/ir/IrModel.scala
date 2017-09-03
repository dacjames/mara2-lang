package io.dac.mara.ir

import scala.collection.generic.CanBuildFrom
import scala.collection.{SeqLike, mutable}


object IrModel {
  case class LVal(value: String) extends AnyVal {
    override def toString: String = value
  }
  case class RVal(value: String) extends AnyVal {
    override def toString: String = value
  }

  def l(value: String) = LVal(value)
  def r(value: String) = RVal(value)

  sealed abstract class Instruction {
    def :+(other: Instruction): Fragment = Fragment(Vector(this, other))
    def value: String
  }
  case class StmtInstruction(value: String) extends Instruction
  case class ExprInstuction(l: LVal, r: RVal) extends Instruction {
    override def value: String = s"${l.value} = ${r.value}"
  }
  case class DefineInstruction(name: String, value: String) extends Instruction

  def stmt(value: String): Instruction = StmtInstruction(value)
  def stmt(l: LVal, r: RVal): Instruction = ExprInstuction(l, r)
  def define(name: String, value: String) = DefineInstruction(name, value)


  sealed trait Fragment extends SeqLike[Instruction, Fragment] {
    def instructions: Seq[Instruction]
    def result: LVal

    def ++(other: Fragment): Fragment
    def :+(instruction: Instruction): Fragment

    def text: String =
      instructions.map(_.value).mkString("\n")

    override def toString(): String = text

    implicit object cbf extends CanBuildFrom[Fragment, Instruction, Fragment] {
      override def apply(from: Fragment): mutable.Builder[Instruction, Fragment] =
        new Builder(state = from.instructions.toBuffer)
      override def apply(): mutable.Builder[Instruction, Fragment] =
        new Builder(state = mutable.Buffer.empty)
    }

    private[this] class Builder(val state: mutable.Buffer[Instruction]) extends mutable.Builder[Instruction, Fragment] {
      override def +=(elem: Instruction): this.type = {
        state += elem
        this
      }

      override def clear(): Unit = {
        state.clear()
      }

      override def result(): Fragment =
        NonEmptyFragemt(state.toVector)
    }

    override protected[this] def newBuilder: mutable.Builder[Instruction, Fragment] =
      new Builder(state = mutable.Buffer.empty)
  }

  object Fragment {
    def apply(instructions: Seq[Instruction]): Fragment =
      if (instructions.isEmpty) EmptyFragment(l(null))
      else NonEmptyFragemt(instructions)
    def apply(l: LVal): Fragment = new EmptyFragment(l)
    def empty: Fragment = EmptyFragment(l(null))
  }

  case class NonEmptyFragemt(instructions: Seq[Instruction]) extends Fragment  {
    require {
      instructions.nonEmpty
    }
    require {
      instructions.exists {
        case _: StmtInstruction => false
        case _: DefineInstruction => true
        case _: ExprInstuction => true
      }

    }

    def result: LVal = {
      instructions.collect {
        case ExprInstuction(l, r) => l
        case DefineInstruction(name, value) => l(name)
      }.last
    }

    override def ++(other: Fragment): Fragment = NonEmptyFragemt(instructions ++ other.instructions)
    override def :+(instruction: Instruction): Fragment = NonEmptyFragemt(this.instructions :+ instruction)

    override def length: Int = instructions.length
    override def apply(idx: Int): Instruction = instructions.apply(idx)
    override def iterator: Iterator[Instruction] = instructions.iterator


    override def seq: Seq[Instruction] = this.instructions
  }

  case class EmptyFragment(l: LVal) extends Fragment {
    private[this] val state = Seq.empty

    override def length: Int = 0
    override def seq: Seq[Instruction] = state
    override def iterator: Iterator[Instruction] = state.iterator


    override def apply(idx: Int): Instruction = throw new IndexOutOfBoundsException()

    override def instructions: Seq[Instruction] = state
    override def result: LVal = {
      if (this.l == null) throw new NoSuchElementException()
      else this.l
    }
    override def ++(other: Fragment): Fragment = other
    override def :+(instruction: Instruction): Fragment = NonEmptyFragemt(Vector(instruction))
  }

  implicit def instructionToFragment(instruction: Instruction): Fragment =
    NonEmptyFragemt(Vector(instruction))


}







