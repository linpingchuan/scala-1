
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

// generated by genprod on Wed Jan 03 17:36:14 CET 2007

package scala

/** Tuple6 is the canonical representation of a @see Product6 */
case class Tuple6[+T1, +T2, +T3, +T4, +T5, +T6](_1:T1, _2:T2, _3:T3, _4:T4, _5:T5, _6:T6) 
  extends Product6[T1, T2, T3, T4, T5, T6]  {

   override def toString() = {
     val sb = new compat.StringBuilder
     sb.append('{').append(_1).append(',').append(_2).append(',').append(_3).append(',').append(_4).append(',').append(_5).append(',').append(_6).append('}')
     sb.toString
   }
}
