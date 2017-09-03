package io.dac.mara.lang.app

import java.io.{ByteArrayInputStream, File, InputStream}

import com.typesafe.scalalogging.LazyLogging
import io.dac.mara.core.MaraAttr._
import io.dac.mara.core.MaraValue.{ErrorValue, ExecutableValue}
import io.dac.mara.core.{NamespaceLookup, _}
import io.dac.mara.phases.{Staged, StagedOp}

import scala.sys.process._


trait StagedApp extends StagedOp with AppAlg[Staged] with NamespaceLookup with ModuleLookup with LazyLogging  {
  def processLogger(info: String, input: String): ProcessLogger =
    new ProcessLogger {
      private[this] def dumpInput = {
        val withLineNumbers = input.split("\n").zipWithIndex.map {
          case (line, i) => s"${i+1}: |${line}"
        }.mkString("\n")

        logger.warn(s"LLVM Input:\n$withLineNumbers")
      }
      private[this] lazy val inputOnce = dumpInput

      override def err(s: => String): Unit = {
        inputOnce
        logger.error(s"Process Error ${info}...\n${s}")
      }
      override def out(s: => String): Unit = {
        inputOnce
        logger.info(s"Process Output ${info}...\n${s}")
      }
      override def buffer[T](f: => T): T = f
    }

  def textInput(text: String): InputStream =
    new ByteArrayInputStream(text.getBytes)

  override def app(name: String, args: Seq[Pair.Value], body: => Seq[Staged]): Staged = op {

    lookupAttr[CodeAttr](name) match {
      case CodeAttr(code) =>
        val assembly = File.createTempFile(s"mara-${name}", ".s")
        val objfile = new File(assembly.getAbsolutePath.replace(".s", ""))
        objfile.createNewFile()

        val fullCode = (getModuleCode ++ code).text

        val p = (Process(s"/usr/local/opt/llvm/bin/llc -o=${assembly}") #< textInput(fullCode)) #&& Process(s"/usr/local/opt/llvm/bin/clang -o ${objfile} ${assembly}")
        p ! processLogger(name, fullCode) match {
          case 0 =>
            objfile.setExecutable(true)
            val executable = ExecutableValue(name, objfile.getAbsolutePath)
            bindAttr(name, ExecutableAttr(executable))
            executable
          case n @ _ =>
            ErrorValue(s"Error staging function ${name}. llc returned exit code ${n}.")
        }
      case ErrorAttr(msg) => ErrorValue(msg)
    }
  }
}
