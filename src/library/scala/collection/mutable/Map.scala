/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/**
 * @since 1
 */
trait Map[A, B] 
  extends Iterable[(A, B)]
     with scala.collection.Map[A, B] 
     with MapLike[A, B, Map[A, B]] {

  override def empty: Map[A, B] = Map.empty

  /** Return a read-only projection of this map.  !!! or just use an (immutable) MapProxy? 
  def readOnly : scala.collection.Map[A, B] = new scala.collection.Map[A, B] {
    override def size = self.size
    override def update(key: A, value: B) = self.update(key, value)
    override def - (elem: A) = self - elem
    override def iterator = self.iterator
    override def foreach[U](f: ((A, B)) =>  U) = self.foreach(f)
    override def empty[C] = self.empty[C]
    def get(key: A) = self.get(key)
  }
  */
}
/* Factory object for `Map` class
 * Currently this returns a HashMap.
 */
object Map extends MutableMapFactory[Map] {
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), Map[A, B], Coll] = new MapBuilderFactory[A, B]
  def empty[A, B]: Map[A, B] = new HashMap[A, B]
}

