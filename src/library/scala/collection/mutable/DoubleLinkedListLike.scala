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
trait DoubleLinkedListLike[A, This >: Null <: DoubleLinkedListLike[A, This]] extends LinkedListLike[A, This] {
  self: This =>

  protected var _prev: This = _
  def prev: This = if (_prev ne null) _prev else throw new NoSuchElementException()

  override def append(that: This) {
    if (!that.isEmpty) {
      if (isEmpty) {
	elem = that.elem
	next = that.next
	next._prev = self
      } else {
	val last = lastElementNode
	last.next = that
	that._prev = last
      }
    }
  }

  override def insert(that: This) {
    if (!that.isEmpty) {
      next._prev = that
      super.insert(that)
      that._prev = self
    }
  }

  override def head_=(e: A) {
    if (isEmpty) {
      next = makeEmpty
      _prev = makeEmpty
    }
    elem = e
  }
  override def tail_=(that: This) {
    if (isEmpty) throw new NoSuchElementException("cannot set the tail of an empty list")
    if (that eq null) throw new NullPointerException("tail cannot be null, use an empty list instead")
    if (!next.isEmpty) {
      next.prev = makeEmpty
      next.prev.next = next
    }
    next = that
    that.prev = self
  }
  def prev_=(that: This) {
    
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
