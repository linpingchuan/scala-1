package scala.tools.nsc
package interactive

import scala.collection._

import scala.tools.nsc.reporters.{Reporter, ConsoleReporter}
import util.FakePos

import dependencies._
import io.AbstractFile

trait BuildManager {

  /** Add the given source files to the managed build process. */
  def addSourceFiles(files: Set[AbstractFile])

  /** Remove the given files from the managed build process. */
  def removeFiles(files: Set[AbstractFile])

  /** The given files have been modified by the user. Recompile
   *  them and their dependent files.
   */
  def update(added: Set[AbstractFile], removed: Set[AbstractFile])

  /** Notification that the supplied set of files is being built */
  def buildingFiles(included: Set[AbstractFile]) {}
  
  /** Load saved dependency information. */
  def loadFrom(file: AbstractFile, toFile: String => AbstractFile) : Boolean
  
  /** Save dependency information to `file'. */
  def saveTo(file: AbstractFile, fromFile: AbstractFile => String)

  def compiler: scala.tools.nsc.Global
}


/** Simple driver for testing the build manager. It presents
 *  the user to a 'resident compiler' prompt. Each line is
 *  interpreted as a set of files that have changed. The builder
 *  then derives the dependent files and recompiles them.
 */
object BuildManagerTest extends EvalLoop {

  def prompt = "builder > "

  def error(msg: String) {
    println(msg + "\n  scalac -help  gives more information")
  }

  def main(args: Array[String]) {
    implicit def filesToSet(fs: List[String]): Set[AbstractFile] =
      Set.empty ++ (fs map AbstractFile.getFile)

    val settings = new Settings(error)
    val command = new CompilerCommand(args.toList, settings, error, false)
//    settings.make.value = "off"
//    val buildManager: BuildManager = new SimpleBuildManager(settings)
    val buildManager: BuildManager = new RefinedBuildManager(settings)

    buildManager.addSourceFiles(command.files)

    // enter resident mode
    loop { line =>
      val args = line.split(' ').toList
      val command = new CompilerCommand(args, new Settings(error), error, true)
      buildManager.update(command.files, Set.empty)
    }

  }
}
