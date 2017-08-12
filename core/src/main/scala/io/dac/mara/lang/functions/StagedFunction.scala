package io.dac.mara.lang.functions

import java.io.{ByteArrayInputStream, File, InputStream}

import com.typesafe.scalalogging.LazyLogging
import io.dac.mara.core._
import io.dac.mara.core.MaraAttr._
import io.dac.mara.core.MaraValue.{ErrorValue, ExecutableValue, StringValue}
import io.dac.mara.core.NamespaceLookup
import io.dac.mara.phases.{Staged, StagedOp}

import scala.sys.process._


trait StagedFunction extends StagedOp with FunctionAlg[Staged] with NamespaceLookup with LazyLogging  {

  def processLogger(info: String): ProcessLogger =
    new ProcessLogger {
      override def err(s: => String): Unit = logger.error(s"Process Error ${info}...\n${s}")
      override def out(s: => String): Unit = logger.info(s"Process Output ${info}...\n${s}")
      override def buffer[T](f: => T): T = f
    }

  def textInput(text: String): InputStream =
    new ByteArrayInputStream(text.getBytes)

  override def defconcrete(name: String, typeparams: Seq[Pair.Type], valparams: Seq[Pair.Value], typex: Option[String], body: Seq[Staged]): Staged = op {
    lookupAttr(name, "code") match {
      case CodeAttr(code) =>
        val hack = code ++ "\n" ++
          """define i32 @main(i32, i8**) #0 {
            |  %3 = alloca i32, align 4
            |  %4 = alloca i8**, align 8
            |  store i32 %0, i32* %3, align 4
            |  store i8** %1, i8*** %4, align 8
            |  ret i32 1
            |}""".stripMargin


        val assembly = File.createTempFile(s"mara-${name}", ".s")
        val objfile = new File(assembly.getAbsolutePath.replace(".s", ""))
        objfile.createNewFile()

        val p = (Process(s"/usr/local/opt/llvm/bin/llc -o=${assembly}") #< textInput(hack)) #&& Process(s"/usr/local/opt/llvm/bin/clang -o ${objfile} ${assembly}")
        p ! processLogger(name) match {
          case 0 =>
            objfile.setExecutable(true)
            val executable = ExecutableValue(name, objfile.getAbsolutePath)
            bindAttr(name, "executable", ValueAttr(executable))
            executable
          case n @ _ =>
            ErrorValue(s"Error staging function ${name}. llc returned exit code ${n}.")
        }
      case ErrorAttr(msg) => ErrorValue(msg)
    }
  }

  override def call(name: String, args: Seq[Staged]): Staged = op {
    lookupAttr(name, "executable") match {
      case ValueAttr(ExecutableValue(_, path)) =>
        val output = Process(path) !! processLogger(name)
        StringValue(output)
      case ErrorAttr(msg) =>
        ErrorValue(msg)
    }
  }
}
