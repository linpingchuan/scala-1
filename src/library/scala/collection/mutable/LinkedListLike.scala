/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import scala.collection.{ LinearSequenceLike, TraversableLike }
import annotation.tailrec
import scala.{ collection => col }

/** This extensible class may be used as a basis for implementing linked
 *  list. Type variable <code>A</code> refers to the element type of the
 *  list, type variable <code>This</code> is used to model self types of
 *  linked lists.
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait LinkedListLike[A, This >: Null <: LinkedListLike[A, This]]
      extends LinearSequenceLike[A, This] {
  self: This =>
  
  protected var elem: A = _
  protected var next: This = _

  protected def clearElem() { elem = null.asInstanceOf[A] }

  protected def makeEmpty: This
  protected def makeFromSequence(seq: col.Sequence[A]): This
  //TODO: why can't I have concrete implementations???
  //protected def newBuilder: Builder[A, This]

  override def isEmpty = next eq null

  override def length: Int = {
    @tailrec
    def loop(x: This, cnt: Int): Int = if (x.isEmpty) cnt else loop(x.next, cnt + 1)
    loop(self, 0)
  }

  /** append <code>that</code> to the end of the list */
  def append(that: This) {
    if (!that.isEmpty) {
      if (isEmpty) {
	elem = that.elem
	next = that.next
      } else {
	val last = lastElementNode
	last.next = that
      }
    }
  }

  /** insert <code>that</code> immediately after this node */
  def insert(that: This) {
    if (!that.isEmpty) {
      that.append(next)
      next = that
    }
  }

  def +=(elem: A): This = {
    val node = makeEmpty
    node.elem = elem
    append(node)
    self
  }


  def ++=(elems: col.Sequence[A]): This = {
    val list = makeFromSequence(elems)
    append(list)
    self
  }

  def clear() {
    next = null
    clearElem()
  }


  protected def pop(): This = {
    val resultNode = tail
    val resultVal = head
    if (tail.isEmpty) {
      elem = null.asInstanceOf[A]
      next = null
    } else {
      elem = tail.head
      next = tail.tail
    }
    resultNode.elem = resultVal
    resultNode
  }

  protected def removeFirst(p: A => Boolean): This = {
    @tailrec
    def loop(xs: This): This = {
      if (xs.isEmpty) makeEmpty // we don't want the terminal node of this list floating around
      else if (p(xs.head)) xs.pop()
      else loop(xs.tail)
    }
    loop(self)
  }

  protected def removeAll(p: A => Boolean): This = {
    val result = removeFirst(p)
    def loop(xs: This) {
      if (!xs.isEmpty) {
	val n = xs.tail.removeFirst(p)
	xs.next = n
	loop(n)
      }
    }
    loop(result)
    result
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

  def head: A    = if (!isEmpty) elem else throw new NoSuchElementException("head of an empty list")
  def head_=(e: A) {
    if (isEmpty) next = makeEmpty
    elem = e
  }
  def tail: This = if (!isEmpty) next else throw new NoSuchElementException("list has no elements")
  /** change <code>tail</code> to <code>that</code>
   *  @throws NoSuchElementException if this list is empty
   *  @throws NullPointerException if that is null
   */
  def tail_=(that: This) {
    if (isEmpty) throw new NoSuchElementException("cannot set tail of an empty list")
    if (that eq null) throw new NullPointerException("tail cannot be null, use an empty list instead")
    next = that
  }
  def update(n: Int, x: A) {
    val loc = drop(n)
    if (loc.isEmpty) throw new NoSuchElementException("list has less than " + n + " elements")
    loc.elem = x
  }

  def get(n: Int): Option[A] = {
    val loc = drop(n)
    if (loc.isEmpty) None else Some(loc.elem)
  }

/*
  override def iterator: Iterator[A] = new Iterator[A] {
    var elems = self
    def hasNext = elems.isEmpty
    def next = {
      val res = elems.head
      elems = elems.tail
      res
    }
  }
*/
/*
  override def foreach[B](f: A => B) {
    @tailrec
    def loop(xs: This): Unit = if (!xs.isEmpty) {
      f(xs.head)
      loop(xs.tail)
    }
    loop(self)
  }
*/
}
