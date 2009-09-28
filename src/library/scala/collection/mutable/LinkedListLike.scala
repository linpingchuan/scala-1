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
  


  protected def clearElem() { _elem = null.asInstanceOf[A] }
  //TODO: why can't I have concrete implementations???
  //protected def newBuilder: Builder[A, This]

  override def isEmpty = next eq null

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

  def clear() {
    next = null
    clearElem()
  }


  protected def pop(): This = {
    val resultNode = tail
    val resultVal = head
    if (tail.isEmpty) {
      _elem = null.asInstanceOf[A]
      _next = null
    } else {
      _elem = tail.head
      _next = tail.tail
    }
    resultNode._elem = resultVal
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

  override def head: A = if (!isEmpty) _elem else throw new NoSuchElementException("head of an empty list")
  def head_=(e: A) {
    if (isEmpty) next = makeEmpty
    elem = e
  }
  override def tail: This = if (!isEmpty) _next else throw new NoSuchElementException("list has no elements")

  def tail_=(that: This) {
    if (isEmpty) throw new NoSuchElementException("cannot set tail of an empty list")
    if (that eq null) throw new NullPointerException("tail cannot be null, use an empty list instead")
    next = that
  }
}
