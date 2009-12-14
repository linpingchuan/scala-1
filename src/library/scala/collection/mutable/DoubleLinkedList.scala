/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.{ generic => gen }
import scala.{ collection => col }

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @author Erik Engbrecht
 *  @version 2.8
 *  @since   1
 */
@serializable @SerialVersionUID(419155950203746706L)
class DoubleLinkedList[A] extends Seq[A]
                             with col.LinearSeq[A]
                             with gen.GenericTraversableTemplate[A, DoubleLinkedList]
                             with DoubleLinkedListLike[A, DoubleLinkedList[A]] { self =>
  def this(next: DoubleLinkedList[A], prev: DoubleLinkedList[A]) {
    this()
    _next = next
    _prev = prev
  }
  def this(v: A) {
    this()
    _elem = v
    _next = makeEmpty
    _next._prev = self
    _prev = makeEmpty
    _prev._next = self
  }
  override def companion: gen.GenericCompanion[DoubleLinkedList] = DoubleLinkedList
  override protected def makeEmpty = new DoubleLinkedList[A]

  protected def makeFromTraversable(seq: col.Traversable[A]) = {
    val builder = DoubleLinkedList.newBuilder[A]
    builder ++= seq
    builder.result()
  }
}

object DoubleLinkedList extends gen.SeqFactory[DoubleLinkedList] {
  implicit def canBuildFrom[A]: gen.CanBuildFrom[Coll, A, DoubleLinkedList[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, DoubleLinkedList[A]] = new Builder[A, DoubleLinkedList[A]] {
    var current = new DoubleLinkedList[A]
    def +=(elem: A): this.type = {
      current += elem //this will be really slow
      this
    }
    def clear() { current = new DoubleLinkedList[A] }
    def result() = current
  }
}
