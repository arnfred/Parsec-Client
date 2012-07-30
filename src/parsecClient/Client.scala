package parsecClient

import java.io._
import java.lang.Thread
import java.lang.reflect
import java.lang.reflect.InvocationTargetException
import java.awt.GraphicsConfiguration
import swing._
import scala.util.parsing.combinator.debugging.AndOrZipper
import scala.util.parsing.combinator.debugging.Controllers
import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.io.{PlainDirectory, Directory, PlainFile}

object Client extends SimpleSwingApplication {
  val c = new Client
  val compile = Button("Compile") { c.init; }
  val step = Button("Step") { val z = c.step; set(z) }
  val stepBack = Button("Step Back") { val z = c.stepBack; set(z) }

  val buttons = new FlowPanel(compile, step, stepBack)
  val text = new ListView(List("Nothing to display yet"))
  val topBox = new BoxPanel(Orientation.Vertical) {
    contents += buttons
    contents += text
  }

  def set(z : AndOrZipper) {
    text.listData = z.toString.split('\n')
    text.repaint
  }

  def top = {
    // Disable step and step-back
    step.enabled_=(false)
    stepBack.enabled_=(false)
    new MainFrame(null) {
      title = "Parsec Debugger"
      //contents = new FlowPanel(compile, step, stepBack)
      contents = topBox
    }
  }
}

class Client extends Controllers {

  val controller = new Controller // this will serve as our way of communicating with the running debugger session
  val req = new Request
  var zs : List[AndOrZipper] = Nil
  var isDone : Boolean = false
  var index : Int = 0
  val methHandler = null
  var op : Thread = null 
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

  def init : Unit = {
    val props = new java.util.Properties()
    props.load(new java.io.FileInputStream("local.properties"))
    val x = props.getProperty("scala.home")
    //IO.load(props, f / "local.properties")
    //val x = props.getProperty("scala.home")

    // Compile files
    val files = compile // Echoed out to save a bit of time

    println("Compile was successful")

    // Now find the class containing the main function
    val classToRun = findClass

    println("Class name: " + classToRun.getName)

    // Create a controller
    controller.request = req

    // Invoke the class we found, calling run with a newly created controller
    val methHandler = classToRun.getMethod("runMain", classOf[Controller]) // runTest would be defined in Parsers and would add Controller argument to the list of listeners
    val f           = classToRun.getField("MODULE$")
    f.setAccessible(true)
    val c           = f.get(null)
    op              = new Thread() {
      override def run() {
        try {
          methHandler.invoke(c, controller)
        }
        catch { case e => e.getCause().printStackTrace(); }
      } 
    }

    // Enable clicking next
    Client.step.enabled_=(true)
    Client.compile.enabled_=(false)

    op.start()
    //testLoop(op, controller)

  }

  def step : AndOrZipper = {

    if (index == 0) {
      // If the index is 0, then we need to get the next element
      next

      // if this was the last, then disable next
      if (isDone) Client.step.enabled_=(false)
    } 
    else {
      index = index - 1

      // If we are moving to the end and it's last, disable next
      if (isDone && index == 0) Client.step.enabled_=(false)
    }
    Client.stepBack.enabled_=(true)
    zs.drop(index).head
  }

  def stepBack : AndOrZipper = {

    if ((index + 1) == zs.length) {
      // If the index is equal to the length of the list, then we can't go any futher back
      // Should call to textfield and change it to "Already at first item"
    }
    else {
      index = index + 1
      // If index is bigger than length - 1, then we don't change the index
      // Should call to textfield and change it to "Already at first item"

      // if we are now at the first then disable prev
      if (index + 1 == zs.length) Client.stepBack.enabled_=(false)
    }
    Client.step.enabled_=(true)
    zs.drop(index).head
  }


  def next : Unit = {
    controller.synchronized {
      controller.notify
    }
    if (op.getState != java.lang.Thread.State.TERMINATED) {
      controller.request.synchronized {
        while (controller.request.field == null) controller.request.wait
        // Now that we are back, get the zipper and reset the controller
        zs = controller.request.field :: zs
        isDone = controller.request.isDone
        controller.request.field = null
      }
    }
  }


  def testLoop(op : Thread, c : Controller) : Unit = {
    println("testLoop: enter")
    c.synchronized {
      c.notify
    }
    if (op.getState != java.lang.Thread.State.TERMINATED) {
      c.request.synchronized {
        while (c.request.field == null) c.request.wait
        // Now that we are back, get the zipper and reset the controller
        zs = c.request.field :: zs
        c.request.field = null
      }


      // print out the zipper
      println(zs.head.toString)

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
        val classes = (for (c <- classStrings if c.last == '$') yield Class.forName(c)).filter(hasRun(_))
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
