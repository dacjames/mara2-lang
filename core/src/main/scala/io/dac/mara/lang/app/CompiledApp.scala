package io.dac.mara.lang.app

import io.dac.mara.core.MaraAttr.CodeAttr
import io.dac.mara.core.{NamespaceLookup, Pair}
import io.dac.mara.ir.IrFragmentVector
import io.dac.mara.phases.{Compiled, CompiledOp}

trait CompiledApp extends CompiledOp with AppAlg[Compiled] with NamespaceLookup {
  import io.dac.mara.ir.implicits._

  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Compiled]): Compiled = op {
    if (args.nonEmpty) ???
    else {
      val bytecode = body.flatMap(_.bytecode)
      val result = body.lastOption match {
        case Some(last) => last.result.s
        case None => "0"
      }

      val main = s"""define i32 @main(i32, i8**) #0 {
              |  %90 = alloca i32, align 4
              |  %91 = alloca i8**, align 8
              |  store i32 %0, i32* %90, align 4
              |  store i8** %1, i8*** %91, align 8
              |  """.stripMargin ++
              bytecode.mkString("\n") ++
              s" ret i32 $result"

      bindAttr(name, "code", CodeAttr(main))

      (IrFragmentVector(main).v, "main")
    }


  }
}
