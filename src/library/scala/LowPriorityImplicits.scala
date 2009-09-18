/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Predef.scala 18558 2009-08-24 14:03:30Z moors $


package scala

import collection.mutable._
import collection.immutable.WrappedString

/** The `LowPriorityImplicits` class provides implicit values that are
 *  valid in all Scala compilation units without explicit qualification, but that
 *  are partially overridden by higher-priority conmversions in Predef
 */
class LowPriorityImplicits {

  implicit def genericWapArray[T](xs: Array[T]): WrappedArray[T] = (xs: AnyRef) match { // !!! drop the AnyRef and get unreachable code errors!
    case x: Array[AnyRef] => wrapArray[AnyRef](x).asInstanceOf[WrappedArray[T]]
    case x: Array[Int] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Double] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Long] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Float] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Char] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Byte] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Short] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Boolean] => wrapArray(x).asInstanceOf[WrappedArray[T]]
    case x: Array[Unit] => wrapArray(x).asInstanceOf[WrappedArray[T]]
  }

  implicit def wrapArray[T <: AnyRef](xs: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](xs.asInstanceOf[Array[AnyRef]])
  implicit def wrapArray(xs: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(xs)
  implicit def wrapArray(xs: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(xs)
  implicit def wrapArray(xs: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(xs)
  implicit def wrapArray(xs: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(xs)
  implicit def wrapArray(xs: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(xs)
  implicit def wrapArray(xs: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(xs)
  implicit def wrapArray(xs: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(xs)
  implicit def wrapArray(xs: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(xs)
  implicit def wrapArray(xs: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(xs)

  implicit def wrapString(s: String): WrappedString = new WrappedString(s)
  implicit def unwrapString(ws: WrappedString): String = ws.self

}
