/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package generic

import mutable.Builder

/** A base class for builder factories
 *
 *  @since 2.8
 */
trait BuilderFactory[-Elem, +To, -From] {

  /** Creates a new builder */
  def apply(from: From): Builder[Elem, To]
}
