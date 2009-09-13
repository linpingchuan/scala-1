/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.generic._

/** <code>Queue</code> objects implement data structures that allow to
 *  insert and retrieve elements in a first-in-first-out (FIFO) manner.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
@serializable @cloneable
class Queue[A] extends LinearSequence[A] 
                  with TraversableClass[A, Queue]
                  with LinkedListTemplate[A, Queue[A]]
                  with Cloneable[Queue[A]] {

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
  def dequeueAll(p: A => Boolean): Sequence[A] = removeAll(p)



  /** Returns the first element in the queue, or throws an error if there
   *  is no element contained in the queue.
   *
   *  @return the first element.
   */
  def front: A = head
}
