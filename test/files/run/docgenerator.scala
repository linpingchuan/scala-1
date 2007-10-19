object Test {
  import java.io.{File, FileWriter}

  /** Tests the generation of the HTML documentation for some Scala
   *  code samples (see value 'code' below) with different scaladoc
   *  options (currently -access:<value>).
   *
   *  @author Stephane Micheloud
   */
  def main(args: Array[String]) {
    // overwrites value of UrlContext.generator in file DocUtil.scala
    System.setProperty("doc.generator", "scaladoc")
    var dirname = System.getProperty("scalatest.output")
    if (dirname eq null) dirname = System.getProperty("java.io.tmpdir")
    val tmpDir = new File(dirname)
    tmpDir.mkdirs()
    val inFile = {
      val f = new File(tmpDir.getPath, "docgenerator_.scala")
      val writer = new FileWriter(f)
      writer.write(code, 0, code.length)
      writer.close
      f
    }
    def createDir(parent: File, dirname: String): File = {
      val outDir = new File(parent, dirname)
      outDir.mkdir
      outDir
    }
    testOptions(inFile, createDir(tmpDir, "test1"), "") // none (default is -access:protected)
    testOptions(inFile, createDir(tmpDir, "test2"), "-access:public")
    testOptions(inFile, createDir(tmpDir, "test3"), "-access:protected")
    testOptions(inFile, createDir(tmpDir, "test4"), "-access:private")
  }

  private def testOptions(inFile: File, outDir: File, opts: String*) {
    val args = Array.concat(Array("-Ydoc", "-d", outDir.getPath, inFile.getPath), opts.toArray:Array[String])
    if (MainDoc.main0(args)) {
      for (name <- List("all-classes.html", "index.html")) {
        val f = new File(outDir, name)
        println(name + ": " + generateMD5Sum(f))
      }
      println
    }
  }

  object MainDoc {
    import scala.tools.nsc._
    import scala.tools.nsc.doc.DocDriver
    import scala.tools.nsc.reporters.ConsoleReporter
    def error(msg: String) { Console.err.println(msg) }
    var reporter: ConsoleReporter = _
    def process(args: Array[String]) {
      val settings = new Settings(error)
      reporter = new ConsoleReporter(settings)
      val command = new CompilerCommand(List.fromArray(args), settings, error, false)
      try {
        object compiler extends Global(command.settings, reporter)
        if (reporter.hasErrors) {
          reporter.flush()
          return
        }
        val run = new compiler.Run
        run compile command.files
        object generator extends DocDriver {
          lazy val global: compiler.type = compiler
          def settings = command.settings
        }
        generator process run.units
        reporter.printSummary()
      } catch {
        case ex @ FatalError(msg) =>
          if (command.settings.debug.value)
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
    }
    def main(args: Array[String]) {
      process(args)
      exit(if (reporter.hasErrors) 1 else 0)
    }
    // main returning a status (no exit code)
    def main0(args: Array[String]): Boolean = {
      process(args)
      !reporter.hasErrors
    }
  }

  private def generateMD5Sum(f: java.io.File): String = {
    import java.io._, java.security._
    val digest = MessageDigest.getInstance("MD5")
    val is = new FileInputStream(f)				
    val buffer = new Array[Byte](8192)
    try {
      var read = is.read(buffer)
      while (read > 0) {
        digest.update(buffer, 0, read)
        read = is.read(buffer)
      }
      val hash = digest.digest()
      val buf = new StringBuilder
      for (i <- hash.indices)
        buf.append((hash(i) & 0xFF).toHexString)
      buf.toString
    }
    catch {
      case e: IOException => 
        throw new RuntimeException("Unable to process file for MD5", e)
    }
    finally {
      try {
        is.close();
      }
      catch {
        case e: IOException =>
          throw new RuntimeException("Unable to close input stream for MD5 calculation", e)
      }
    }	
  }

  private val code = """
package examples

abstract class C0 {
  def foo_public
  protected def foo_protected
  private def foo_private {}
  class C1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected class C1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private class C1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

protected abstract class C0_Protected {
  def foo_public
  protected def foo_protected
  private def foo_private {}
  class C1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected class C1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private class C1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

private abstract class C0_Private {
  def foo_public
  protected def foo_protected
  private def foo_private {}
  class C1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected class C1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private class C1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}


object obj0 {
  def bar_public {}
  protected def bar_protected {}
  private def bar_private {}
  object obj1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected object obj1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private object obj1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

protected object obj0_Protected {
  def bar_public {}
  protected def bar_protected {}
  private def bar_private {}
  object obj1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected object obj1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private object obj1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}

private object obj0_Private {
  def bar_public {}
  protected def bar_protected {}
  private def bar_private {}
  object obj1_Public {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  protected object obj1_Protected {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
  private object obj1_Private {
    val x_public = ()
    protected val x_protected = ()
    private val x_private = ()
  }
}
"""
}