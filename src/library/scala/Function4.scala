/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Wed Jun 17 14:10:05 PDT 2009  (with extra methods)

package scala



/** <p>
 *    Function with 4 parameters.
 *  </p>
 *  
 */
trait Function4[-T1, -T2, -T3, -T4, +R] extends AnyRef { self =>
  def apply(v1:T1,v2:T2,v3:T3,v4:T4): R
  override def toString() = "<function4>"
  
  /** f(x1,x2,x3,x4)  == (f.curry)(x1)(x2)(x3)(x4)
   */
  def curry: T1 => T2 => T3 => T4 => R = {
    (x1: T1) => (x2: T2) => (x3: T3) => (x4: T4) => apply(x1,x2,x3,x4)
  }

}
