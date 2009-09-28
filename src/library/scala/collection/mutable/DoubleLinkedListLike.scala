/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This extensible class may be used as a basis for implementing double
 *  linked lists. Type variable <code>A</code> refers to the element type
 *  of the list, type variable <code>This</code> is used to model self
 *  types of linked lists.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 *  @since   2.8
 */
trait DoubleLinkedListLike[A, This >: Null <: DoubleLinkedListLike[A, This]] extends LinearSequenceLike[A, This] {
  self: This =>

  protected def clearElem() { _elem = null.asInstanceOf[A] }

  protected var _prev: This = _
  def prev: This = if (_prev ne null) _prev else throw new NoSuchElementException()

  def append(that: This) {
    if (!that.isEmpty) {
      if (isEmpty) {
	elem = that._elem
	next = that._next
	next._prev = self
      } else {
	val last = lastElementNode
	last._next = that
	that._prev = last
      }
    }
  }

  def insert(that: This) {
    if (!that.isEmpty) {
      _next._prev = that
      that.append(_next)
      _next = that
      that._prev = self
    }
  }

  def head_=(e: A) {
    if (isEmpty) {
      _next = makeEmpty
      _prev = makeEmpty
    }
    _elem = e
  }
  override def tail_=(that: This) {
    if (isEmpty) throw new NoSuchElementException("cannot set the tail of an empty list")
    if (that eq null) throw new NullPointerException("tail cannot be null, use an empty list instead")
    if (!next.isEmpty) {
      next._prev = makeEmpty
      next._prev._next = _next
    }
    _next = that
    that._prev = self
  }
  def prev_=(that: This) {
    if (that eq null) throw new NullPointerException("prev cannot be set to null")
    if (!_prev.isEmpty) {
      _prev._next = makeEmpty
      _prev._next._prev = _prev
    }
    _prev = that
    that._next = self
  }

  def clear() {
    if (!isEmpty) {
      if (!_prev.isEmpty) {
	_prev._next = makeEmpty
	_prev._next._prev = _prev
      } else {
	_prev._next = null
      }
      _prev = null
      if (!_next.isEmpty) {
	_next._prev = makeEmpty
	_next._prev._next = _next
      } else {
	_next._prev = null
      }
      _next = null
    }
  }

  def remove() {
    if (next ne null)
      next._prev = _prev
    if (_prev ne null)
      _prev.next = next
    _prev = null
    next = makeEmpty
  }
}
