package parsecClient

import java.io._
import java.lang.Thread
import java.lang.reflect
import java.lang.reflect.InvocationTargetException
import scala.util.parsing.combinator.debugging.AndOrZipper
import scala.util.parsing.combinator.debugging.Controllers
import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.io.{PlainDirectory, Directory, PlainFile}

object Client {
  def main(args : Array[String]) : Unit = {
    val c = new Client
    c.run
  }
}

class Client extends Controllers {


  var z : AndOrZipper = null

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

  def run : Unit = {
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

    println("Compile was successful")

    // Now find the class containing the main function
    val classToRun = findClass

    println("Class name: " + classToRun.getName)

    // Create a controller
    val controller = new Controller // this will serve as our way of communicating with the running debugger session
    val req = new Request
    controller.request = req

    // Invoke the class we found, calling run with a newly created controller
    println(classToRun.getDeclaredMethods.map(m => m.getName).mkString("\n"))
    val methHandler = classToRun.getMethod("runMain", classOf[Controller]) // runTest would be defined in Parsers and would add Controller argument to the list of listeners
    val f = classToRun.getField("MODULE$")
    f.setAccessible(true)
    val c = f.get(null)

    // create thread
    val op = new Thread() {

      override def run() {
        methHandler.invoke(c, controller)
      } 
    }
    op.start()
 //   controller.synchronized {
 //     methHandler.invoke(c, controller) // run in another thread
//      println("0")

      // Now we go into a gui test loop
      testLoop(op, controller)
 //   }

  }

  def testLoop(op : Thread, c : Controller) : Unit = {
    println("testLoop: enter")
    c.synchronized {
      c.notify
    }
    println("2.5")
    if (op.getState != java.lang.Thread.State.TERMINATED) {
      c.request.synchronized {
        while (c.request.field == null) c.request.wait
        // Now that we are back, get the zipper and reset the controller
        z = c.request.field
        c.request.field = null
      }


      // print out the zipper
      println(z.toString)

      // Check if we should loop around
      println("\n>> ")
      scala.Console.readChar match {
        case 'q'    => op.stop
        case 's'    => testLoop(op, c)
        case _      => println("Press q to quit"); testLoop(op, c)
      }
    }
  }


  def findClass : Class[_] = {
    def findClass0(dir : File) : List[Class[_]] = {
      if (dir.isDirectory) {
        val classPointers = dir.listFiles.filter(f => """.*\.class$""".r.findFirstIn(f.getName).isDefined).toList
        val directories = dir.listFiles.filter(f => f.isDirectory).toList
        val classStrings = classPointers.map(c => c.getPath.split('.').head.split('/').drop(1).mkString("."))
        println(classStrings)
        //val classes = (for (c <- classStrings if c.last == '$') yield Class.forName(c)).filter(hasRun(_))
        val classes = (for (c <- classStrings) yield Class.forName(c)).filter(hasRun(_))
        return classes ++ directories.flatMap(findClass0)
      }
      else throw new Exception(dir + " is not a directory")
    }

    def hasRun(c : Class[_]) : Boolean = {
      (c.getDeclaredMethods.filter(m => m.getName == "runMain").length == 1)
    }

    val cs = findClass0(new File("build"))
    println(cs)
    cs match {
      case head::_   => head
      case _            => throw new Exception("No runDebug class in uploaded files")
    }
  }
}
