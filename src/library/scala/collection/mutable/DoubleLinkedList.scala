/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @SerialVersionUID(419155950203746706L)
class DoubleLinkedList[A] (var next: DoubleLinkedList[A], var _prev: DoubleLinkedList[A]) 
  extends LinearSequence[A]
     with GenericTraversableTemplate[A, DoubleLinkedList]
     with DoubleLinkedListLike[A, DoubleLinkedList[A]] {
  def this() = this(null, null)
  def this(v: A) {
    this()
    elem = v
    next = makeEmpty
    _prev = makeEmpty
  }
  override def companion: GenericCompanion[DoubleLinkedList] = DoubleLinkedList
  override protected def makeEmpty = new DoubleLinkedList[A]
}

object DoubleLinkedList extends SequenceFactory[DoubleLinkedList] {
  implicit def builderFactory[A]: BuilderFactory[A, DoubleLinkedList[A], Coll] = //new BuilderFactory[A, DoubleLinkedList[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
    new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, DoubleLinkedList[A]] = new Builder[A, DoubleLinkedList[A]] {
    var current = new DoubleLinkedList[A]
    def +=(elem: A): this.type = {
      current.append(new DoubleLinkedList(elem))
      this
    }
    def clear() { current = new DoubleLinkedList[A] }
    def result() = current
  }
}
