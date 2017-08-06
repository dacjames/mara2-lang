package io.dac.mara.core


object Builtins {
  import MaraType._
  import MaraValue._

  lazy val types = Map(
    "Any" -> AnyType(),
    "String" -> StringType(),
    "Int" -> IntType(),
    "Bool" -> BoolType()
  )

  lazy val values = Map(
    "true" -> BoolValue(true),
    "false" -> BoolValue(false)
  )

  require {
    types.forall {
      case (name, typex) => typex.name.contains(name)
    }
  }
}
