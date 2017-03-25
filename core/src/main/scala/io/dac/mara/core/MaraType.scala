package io.dac.mara.core

sealed trait MaraType extends MaraRoot {
  def name: Option[String]
}
object MaraType {
  case class AnyType() extends MaraType {
    val name = Some("Any")
  }
  case class BoolType() extends MaraType {
    val name = Some("Bool")
  }
  case class IntType() extends MaraType {
    val name = Some("Int")
  }
  case class StringType() extends MaraType {
    val name = Some("String")
  }
  case class InferrableType() extends MaraType {
    val name = None
  }
  case class ErrorType(msg: String) extends MaraType {
    val name = None
  }
  case class UnitType() extends MaraType {
    val name = Some("Unit")
  }
  case class EmptyType() extends MaraType {
    val name = None
  }
  sealed trait LiteralType[T] extends MaraType {
    val name = None
    def value: T
  }

  case class StringLiteralType(value: String) extends LiteralType[String] {
    override def toString = s"'$value'"
  }
  case class IntLiteralType(value: Int) extends LiteralType[Int] {
    override def toString = value.toString
  }
  case class BoolLiteralType(value: Boolean) extends LiteralType[Boolean] {
    override def toString = value.toString
  }

  case class TagType(key: LiteralType[_], value: MaraType) extends MaraType {
    val name = None
  }

  case class RecordType(fields: Seq[MaraType]) extends MaraType {
    val name = None

    private[MaraType] def compareWith(other: RecordType)(pred: (MaraType, MaraType) => Boolean): Boolean =
      this.fields.length == other.fields.length &&
        this.fields.zip(other.fields).forall(p => pred(p._1, p._2))

    private[MaraType] def positionalSubtype(other: RecordType): Boolean = {
      if (this.fields.length != other.fields.length) return false

      var i = 0
      while (i < this.fields.length) {
        this.fields(i) match {
          case TagType(_, thisvalue) =>
            other.fields(i) match {
              case TagType(_, othervalue) =>
                if (!isSubtype(thisvalue, othervalue)) return false
              case _ =>
                return false
            }
          case _ =>
            return false
        }
        i += 1
      }

      true
    }

  }
  case class FunctionType(input: MaraType, output: MaraType) extends MaraType {
    val name = None
  }

  def isSubtype(a: MaraType, b: MaraType): Boolean = (a, b) match {
    case (_, AnyType()) => true
    case (_, ErrorType(_)) => false
    case (a: LiteralType[_], b: LiteralType[_]) => a.value == b.value
    case (a: StringLiteralType, b: StringType) => true
    case (a: IntLiteralType, b: IntType) => true
    case (a: BoolLiteralType, b: BoolType) => true
    case (a: TagType, b: TagType) => isSubtype(a.key, b.key) && isSubtype(a.value, b.value)
    case (a: RecordType, b: RecordType) =>
      a.compareWith(b)(isSubtype) || a.positionalSubtype(b)
    case (a: FunctionType, b: FunctionType) =>
      isSubtype(a.input, b.input) && isSupertype(a.output, b.output)
    case _ => a == b
  }

  def isSupertype(a: MaraType, b: MaraType): Boolean = (a, b) match {
    case (AnyType(), _) => true
    case (ErrorType(_), _) => false
    case (a: RecordType, b: RecordType) => a.compareWith(b)(isSupertype)
    case (a: StringType, b: StringLiteralType) => true
    case (a: IntType, b: IntLiteralType) => true
    case (a: LiteralType[_], b: LiteralType[_]) => a.value == b.value
    case _ => a == b
  }

  def promote(a: MaraType): MaraType = a match {
    case s: StringLiteralType => StringType()
    case i: IntLiteralType => IntType()
    case b: BoolLiteralType => BoolType()
    case _ => a
  }
}

