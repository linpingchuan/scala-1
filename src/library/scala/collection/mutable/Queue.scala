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

import scala.collection.generic.{ SequenceFactory, GenericTraversableTemplate, BuilderFactory }
import scala.{ collection => col }

/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
@serializable @cloneable
class Queue[A] extends Sequence[A] 
                  with GenericTraversableTemplate[A, Queue]
                  with LinkedListLike[A, Queue[A]]
                  with Cloneable[Queue[A]] {

  override def companion = Queue
  protected def makeEmpty = new Queue[A]
  protected def makeFromTraversable(seq: col.Traversable[A]) = {
    val builder = companion.newBuilder[A]
    builder ++= seq
    builder.result()
  }

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*): Unit = this ++= elems

  /** Returns the first element in the queue, and removes this element
   *  from the queue.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the first element of the queue.
   */
  def dequeue(): A = pop().head

  /** Returns the first element in the queue which satisfies the
   *  given predicate, and removes this element from the queue.
   *
   *  @param p   the predicate used for choosing the first element
   *  @return the first element of the queue for which p yields true
   */
  def dequeueFirst(p: A => Boolean): Option[A] = {
    val r = removeFirst(p)
    if (r.isEmpty) None else Some(r.head)
  }

  /** Returns all elements in the queue which satisfy the
   *  given predicate, and removes those elements from the queue.
   *
   *  @param p   the predicate used for choosing elements
   *  @return    a sequence of all elements in the queue for which
   *             p yields true.
   */
  def dequeueAll(p: A => Boolean): Queue[A] = removeAll(p)



  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   */
  def front: A = head
}

object Queue extends SequenceFactory[Queue] {
  implicit def builderFactory[A]: BuilderFactory[A, Queue[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A] = new Builder[A, Queue[A]] {
    var front: Queue[A] = _
    var back: Queue[A] = _
    clear() // initializes front and back
    def +=(elem: A): this.type = {
      back += (elem)
      back = back.tail
      this
    }
    def clear() {
      front = new Queue[A]
      back = front
    }
    def result() = front
  }
}
