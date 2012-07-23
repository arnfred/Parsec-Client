package parsecClient

import java.io._
import java.lang.reflect

object Client {

  import scala.tools.nsc
  import scala.tools.nsc.reporters.ConsoleReporter
  import scala.tools.nsc.io.{PlainDirectory, Directory, PlainFile}

  def compile : List[String] = {

    def createCompiler(out: String): (nsc.Global, nsc.Settings) = {
      val settings = new nsc.Settings()
      val props = new java.util.Properties()
      props.load(new java.io.FileInputStream("local.properties"))
      val classPath = props.getProperty("scala.home") + "/lib/scala-library.jar"
      settings.classpath.value = classPath //System.getProperty("java.class.path")
      val jFile = new java.io.File(out)
      val directory = new Directory(jFile)
      val compDirectory = new PlainDirectory(directory)
      settings.outputDirs.setSingleOutput(compDirectory)
      
      val global = new nsc.Global(settings, new ConsoleReporter(settings))
      (global, settings)
    }
   
    def doCompile(filesToCompile : List[String], dest : String) {
      println("WILL COMPILE: " + filesToCompile.mkString(", "))
      val (comp, settings) = createCompiler(dest)
      val command = new nsc.CompilerCommand(filesToCompile, settings)
      val run = new comp.Run
      run compile command.files
    }

    // Get file handle of original file or directory
    val dir = "resources"
    val build = "build"
    val orig = new File(dir)
    var error : Option[String] = None
    var files : List[File] = Nil
    var fnames : List[String] = Nil
    var fpaths : List[String] = Nil

    // In case it's a directory, let the file array contain all the files of the directory
    if (orig.isDirectory) {
      files     = orig.listFiles.filter(f => """.*\.scala$""".r.findFirstIn(f.getName).isDefined).toList
      fnames    = files.map(f => f.getName)
      fpaths    = fnames.map(f => dir + "/" + f)
    }

    // Then compile the files
    doCompile(fpaths, build)
    return fnames
  }

  def main(args: Array[String]) {
    println("hello world")
    val props = new java.util.Properties()
    props.load(new java.io.FileInputStream("local.properties"))
    val x = props.getProperty("scala.home")
    //IO.load(props, f / "local.properties")
    //val x = props.getProperty("scala.home")
    println(x + "/lib/scala-library.jar")
    println(System.getProperty("java.class.path"))

    // Compile files
    // val files = compile // Echoed out to save a bit of time

    // Now find the class containing the main function
    val c = findClass

    println("Class name: " + c.getName)

    // Make a method that uses
    // Java lang reflect
    // and getClasses()
    // Go through the directories recursively.
    // For each file, do:
    //   c = Class.forName( "name of path, but replace '/' with '.'")
    //   ms = c.getDeclaredMethods() {
    //     check that ms = main, and then run that
    //   }
    // if there is more than one, or less than one, throw an exception
    // new File(Thread.currentThread().getContextClassLoader().GetRessource('place of file').getFile())

  }

  // Def runTest(contr : Controller) {
    // registerController(contr)

  def findClass : Class[_] = {
    def findClass0(dir : File) : List[Class[_]] = {
      if (dir.isDirectory) {
        val classPointers = dir.listFiles.filter(f => """.*\.class$""".r.findFirstIn(f.getName).isDefined).toList
        val directories = dir.listFiles.filter(f => f.isDirectory).toList
        val classStrings = classPointers.map(c => c.getPath.split('.').head.split('/').drop(1).mkString("."))
        val classes = classStrings.map(c => Class.forName(c)).filter(hasMain)
        return classes ++ directories.flatMap(findClass0)
      }
      else throw new Exception(dir + " is not a directory")
    }

    def hasMain(c : Class[_]) : Boolean = c.getName.last != '$' && c.getDeclaredMethods.filter(m => m.getName == "main").length == 1

    val cs = findClass0(new File("build"))
    if (cs.length > 1) throw new Exception("More than one runDebug class in uploaded files")
    if (cs.length == 0) throw new Exception("No runDebug class in uploaded files")

    // Get first and only element
    return cs.head
  }
}
