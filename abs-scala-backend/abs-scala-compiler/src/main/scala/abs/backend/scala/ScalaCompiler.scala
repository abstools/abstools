package abs.backend.scala

import java.io.File
import abs.frontend.parser.Main
import scala.collection.JavaConversions._

class ScalaCompiler extends Main {
  private var targetDir: String = _
  private var sourceOnly = false
  
  val f = File.createTempFile("abs-scala", "")
  f.delete()
  targetDir = f.getAbsolutePath()
  
  /*
  private def generateFileList(f: File): List[String] =
    if (f.isDirectory())
      f.listFiles().foldLeft(Nil)().flatten
    else {
      if (f.getName().endsWith(".scala"))
        f.getAbsolutePath() :: Nil
      else
          Nil
    }
  */
  
  def compile(args: Array[String]) {
    val model = parse(args)
    
    if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
      return
    
    val f = new File(targetDir)
    if (!f.mkdirs()) {
      System.err.println("Unable to create output directory: " + targetDir)
      System.exit(1)
    }
    
    model.generateScala(f);
    
    if (!sourceOnly) {
      
      //scala.tools.nsc.Main.main();
    }
  }
  
  private def handleArgs(args: List[String]): List[String] = args match {
    case Nil => Nil
    case "-d" :: Nil =>
      System.err.println("Please specify output directory")
      System.exit(1)
      Nil
    case "-d" :: dir :: tl =>
      targetDir = dir
      handleArgs(tl)
    case "-sourceonly" :: tl =>
      sourceOnly = true
      handleArgs(tl)
    case s :: tl =>
      s :: handleArgs(tl)
  }
  
  override def parseArgs(args: Array[String]): java.util.List[String] =
    handleArgs(args.toList).reverse
    
  override protected def printUsage() {
    super.printUsage()
    System.out.println("Scala backend:")
    System.out.println("  -d <dir>       outputs generated files to given directory")
    System.out.println("  -sourceonly    do not compile generated class files")
  }
}

object ScalaCompiler {
  def main(args: Array[String]): Unit = {
    new ScalaCompiler().compile(args)
  }
}