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

      val (bytecode, result) = Compiled.recurse(body)


      val main = s"""define i32 @main(i32, i8**) #0 {
              |  %3 = alloca i32, align 4
              |  %4 = alloca i8**, align 8
              |  store i32 %0, i32* %3, align 4
              |  store i8** %1, i8*** %4, align 8
              |  """.stripMargin ++
              bytecode.mkString("\n") ++
              s"\n ret i32 $result \n}"

      bindAttr[CodeAttr](name, CodeAttr(main))

      (IrFragmentVector(main).v, "main")
    }


  }
}
