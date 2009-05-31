/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.actors

import java.lang.Runnable

/** <p>
 *    The class <code>ActorTask</code>...
 *  </p>
 *
 *  @author Philipp Haller
 */
class ActorTask extends Runnable {

  private var a: Actor = null
  private var fun: () => Unit = null

  def this(a: Actor, block: => Unit) {
    this()
    this.a = a
    this.fun = () => { block }
  }

  def run() {
    val saved = Actor.tl.get
    Actor.tl set a
    try {
      if (a.shouldExit) // links
        a.exit()
      try {
        try {
          fun()
        } catch {
          case e: Exception if (a.exceptionHandler.isDefinedAt(e)) =>
            a.exceptionHandler(e)
        }
      } catch {
        case _: KillActorException =>
      }
      a.kill()
    }
    catch {
      case _: SuspendActorException => {
        // do nothing
      }
      case t: Exception => {
        Debug.info(a+": caught "+t)
        a.terminated()
        // links
        a.synchronized {
          if (!a.links.isEmpty)
            a.exitLinked(t)
          else
            t.printStackTrace()
        }
      }
    } finally {
      Actor.tl set saved
      this.a = null
      this.fun = null
    }
  }

}
