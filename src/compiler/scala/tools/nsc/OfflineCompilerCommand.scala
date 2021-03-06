/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

/** A compiler command for the offline compiler.
 *
 * @author Martin Odersky and Lex Spoon
 */
class OfflineCompilerCommand(
  arguments: List[String],
  settings: Settings,
  error: String => Unit, 
  interactive: Boolean) 
extends CompilerCommand(arguments, new Settings(error), error, false)
{
  override val cmdName = "fsc"
  import settings._
  
  disable(prompt)
  disable(resident)
  
  BooleanSetting("-reset",    "Reset compile server caches")
  BooleanSetting("-shutdown", "Shutdown compile server")
  StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "")
  BooleanSetting("-J<flag>",  "Pass <flag> directly to runtime system")
}
