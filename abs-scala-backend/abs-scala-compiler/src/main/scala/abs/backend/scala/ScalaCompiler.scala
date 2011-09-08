/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala

import java.io.File
import abs.frontend.parser.Main
import scala.collection.JavaConversions._

/**
 * 
 * @author Andri Saar <andri@cs.ioc.ee>
 */
class ScalaCompiler extends ScalaBackend {
  sourceOnly = false
  
  private def generateFileList(f: File): List[String] =
    if (f.isDirectory())
      f.listFiles().foldLeft(Nil: List[String])((xs, f) => generateFileList(f) ::: xs)
    else {
      if (f.getName().endsWith(".scala"))
        f.getAbsolutePath() :: Nil
      else
        Nil
    }
  
  override def compileSources() {
    // find continuations JAR from classpath (very crude, I know, sorry)
    val contJar: String = System.getProperty("java.class.path").split(java.io.File.pathSeparator).find(_.matches(".*continuations[^/]*\\.jar")) match {
      case None =>
        System.err.println("Continuations plugin JAR not found in classpath")
        System.exit(1)
        null
      case Some(x) =>
        x
    }
      
    val reporter = new scala.tools.nsc.reporters.ConsoleReporter(new scala.tools.nsc.Settings(System.err.println))
    val compiler = new scala.tools.nsc.Global(reporter)

    compiler.settings.classpath.append(System.getProperty("java.class.path"))
    compiler.settings.bootclasspath.append(System.getProperty("java.class.path"))
    compiler.settings.d.value = outputDir.getAbsolutePath()
    compiler.settings.plugin.appendToValue(contJar)
    compiler.settings.require.appendToValue("continuations")
    compiler.settings.pluginOptions.appendToValue("continuations:enable")

    //compiler.settings.Ylogcp.value = true
    val run = new compiler.Run()
    run compile generateFileList(outputDir)
    reporter.printSummary()
      
    if (reporter.hasErrors)
      System.exit(1)
  }
}

object ScalaCompiler {
  def main(args: Array[String]): Unit = {
    new ScalaCompiler().compile(args)
  }
}