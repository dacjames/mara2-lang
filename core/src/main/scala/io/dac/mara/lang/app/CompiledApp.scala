package io.dac.mara.lang.app

import io.dac.mara.core.MaraAttr.CodeAttr
import io.dac.mara.core.{NamespaceLookup, Pair}
import io.dac.mara.phases.{Compiled, CompiledOp}

trait CompiledApp extends CompiledOp with AppAlg[Compiled] with NamespaceLookup {
  import io.dac.mara.ir.IrModel._

  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Compiled]): Compiled = op {
    if (args.nonEmpty) ???
    else {
      val bodyFragment = Compiled.recurse(body)

      val main =
        (define("main", "define i32 @main(i32, i8**) #0 {") :+
        stmt("%3 = alloca i32, align 4") :+
        stmt("%4 = alloca i8**, align 8") :+
        stmt("store i32 %0, i32* %3, align 4") :+
        stmt("store i8** %1, i8*** %4, align 8")) ++
        bodyFragment :+
        stmt(s"ret i32 ${bodyFragment.result}") :+
        stmt(s"}")

      bindAttr[CodeAttr](name, CodeAttr(main))
      main
    }


  }
}
