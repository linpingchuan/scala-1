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
import annotation.tailrec

trait LinearSequenceLike[A, This >: Null <: LinearSequenceLike[A, This]] extends col.LinearSequenceLike[A, This] { self: This =>
  protected var _elem: A = _
  @deprecated("use head instead")
  def elem = _elem
  @deprecated("assign to head instead")
  def elem_=(e: A): Unit = _elem = e
  protected var _next: This = _
  @deprecated("use tail instead")
  def next: This = _next
  @deprecated("assign to tail instead")
  def next_=(that: This): Unit = _next = that

  protected def makeEmpty: This
  protected def makeFromTraversable(seq: col.Traversable[A]): This

  /**
   * @throws NoSuchElementException if the list is empty 
   */ 
  override def head: A = if (isEmpty) throw new NoSuchElementException else _elem
  def head_=(e: A): Unit
  /**
   * @throws NoSuchElementException if the list is empty
   */
  override def tail: This = if (isEmpty) throw new NoSuchElementException else _next
  /** change <code>tail</code> to <code>that</code>
   *  @throws NoSuchElementException if this list is empty
   *  @throws NullPointerException if that is null
   */
  def tail_=(that: This): Unit
  def append(that: This): Unit
  def insert(that: This): Unit
  def clear(): Unit
  def +=(elem: A): This /* = {
    val node = makeEmpty
    node._elem = elem
    append(node)
    self
  }*/
  def ++=(elems: col.Traversable[A]): This = {
    val list = makeFromTraversable(elems)
    append(list)
    self
  }
  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc.isEmpty) None else Some(loc._elem)
  }
  def update(n: Int, x: A) {
    val loc = drop(n)
    if (loc.isEmpty) throw new NoSuchElementException("list has less than " + n + " elements")
    loc._elem = x
  }
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
  protected def terminalNode: This = {
    @tailrec
    def loop(xs: This): This = if (isEmpty) xs else loop(xs.tail)
    loop(self)
  }


  /**
   * @return the last non-terminal node in the list
   * @throws NoSuchElementException if this list is empty
   */
  protected def lastElementNode: This = {
    @tailrec
    def loop(xs: This): This = if (xs.tail.isEmpty) xs else loop(xs.tail)
    loop(self)
  }
}
