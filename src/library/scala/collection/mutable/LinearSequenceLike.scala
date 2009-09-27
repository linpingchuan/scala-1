/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*                                                                      */

// $Id$

package scala.collection.mutable

import scala.{ collection => col }

trait LinearSequenceLike[A, This >: Null <: LinearSequenceLike[A, This]] extends col.LinearSequenceLike[A, This] { self: This =>
  @deprecated("use tail instead")
  def next: This
  @deprecated("assign to tail instead")
  def next_=(that: This): Unit
  @deprecated("use head instead")
  def elem: A
  @deprecated("assign to head instead")
  def elem_=(e: A): Unit

  /**
   * @throws NoSuchElementException if the list is empty 
   */ 
  def head: A
  def head_=(e: A): Unit
  /**
   * @throws NoSuchElementException if the list is empty
   */
  def tail: This
  /** change <code>tail</code> to <code>that</code>
   *  @throws NoSuchElementException if this list is empty
   *  @throws NullPointerException if that is null
   */
  def tail_=(that: This): Unit
  def append(that: This): Unit
  def insert(that: This): Unit
  def clear(): Unit
  def +=(e: A): This
  def ++=(s: col.Traversable[A]): This
  def get(n: Int): Option[A]
  def update(n: Int, e: A): Unit
  /** obtain the current terminal node for this list
   *  the terminal node is a nil list and does not have a head or a tail,
   *  and will throw <code>NoSuchElementException</code> if you try to
   *  access the head or the tail.
   *
   *  This method is linear with respect to the length of the list.  If
   *  an implementing class adds additional structure that allows the
   *  terminal node to be found more quickly, it should override this
   *  method.
   */
  protected def terminalNode: This = lastElementNode.tail
  /**
   * @return the last non-terminal node in the list
   * @throws NoSuchElementException if this list is empty
   */
  protected def lastElementNode: This
}
