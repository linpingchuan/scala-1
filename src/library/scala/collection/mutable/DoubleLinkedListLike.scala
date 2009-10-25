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
 *  @author  Erik Engbrecht
 *  @version 2.8
 *  @since   2.8
 */
trait DoubleLinkedListLike[A, This >: Null <: DoubleLinkedListLike[A, This]] extends LinearSequenceLike[A, This] {
  self: This =>

  protected def clearElem() { _elem = null.asInstanceOf[A] }

  override def isEmpty = (_next eq null) || (_prev eq null)
  /** returns true if this node is empty and there are not other nodes in the list
   *  <p>if <code>isSentinal</code> is true then <code>isListEmpty</code> is false</p>
   *  <p>if <code>isEmpty</code> is false then <code>isListEmpty</code> is false</p>
   */
  def isListEmpty = (_next eq null) && (_prev eq null)

  /** true if this is the sentinal node at the front of the list
   *  if true, then this node is empty, however unlike other empty nodes
   *  <code>tail</code>/<code>next</code> is defined and thus will not throw a
   *  <code>NoSuchElementException</code> when accessed. However <code>head</code>
   *  and <code>prev</code> will throw throw a <code>NoSuchElementException</code>.
   */
  def isFrontSentinal = (_prev ne null) && (_next eq null)

  /** true if this a the rear sentinal node of the list
   *  if true, then this node is empty, however unlike other empty nodes
   *  <code>prev</code> is defined and thus will not throw a <code>NoSuchElementException</code>
   *  when accessed.  However <code>head</code> and <code>tail</code>/<code>next</code> are
   *  not defined and will throw a <code>NoSuchElementException</code>.
   */
  def isRearSentinal = (_next ne null) && (_prev eq null)

  /** true if this node is a front sentinal or a rear sentinal
   *  Sentinal nodes do not have a defined <code>head</code> and have either a defined
   *  <code>tail</code> (for a front sentinal) or a defined <code>prev</code> (for a rear
   *  sentinal).
   */
  def isSentinal = isFrontSentinal || isRearSentinal

  protected var _prev: This = _
  def prev: This = if (_prev ne null) _prev else throw new NoSuchElementException()
  def prev_=(that: This) {
    if (that eq null) throw new NullPointerException("prev cannot be set to null")
    if (!_prev.isEmpty) {
      _prev._next = makeEmpty
      _prev._next._prev = _prev
    }
    _prev = that
    that._next = self
  }

  def append(that: This) {
    if (!that.isEmpty) {
      if (isEmpty) {
	_elem = that._elem
	_next = that._next
	_next._prev = self
      } else {
	val last = lastElementNode
	last._next = that
	that._prev = last
      }
    }
  }

  def +=(e: A): This = {
    if (isEmpty) {
      _next = makeEmpty
      _next._prev = self
      _prev = makeEmpty
      _prev._next = self
      _elem = e
    } else {
      val last = lastElementNode
      val node = makeEmpty
      node._elem = e
      last._next._prev = node
      node._next = last._next
      last._next = node
      node._prev = last
    }
    self
  }

  def insert(that: This) {
    if (!that.isEmpty) {
      if (isRearSentinal) {
	// this is the rear sentinal node of the list, it is impossible to insert after the end
	// instead, set this node's head to that's head and make it
	head = that.head
	if (!that.tail.isEmpty) {
	  _next = that._next
	  _next._prev = self
	}
	// this node has stolen that node's data, so make sure it can no longer
	// reference it
	that.clear() 
      } else {
	// this node is a regular node or 
	_next._prev = that
	that.append(_next)
	_next = that
	that._prev = self
      }
    }
  }

  def head_=(e: A) {
    // if this is a sentinal node then setting the head will make it so that it
    // can no longer be a sentinal node, and therefore new sentinal nodes must
    // be created as appropriate
    if (isFrontSentinal)  newFrontSentinal()    // create a new front sentinal to replace this one
    else if (isRearSentinal) newRearSentinal()  // create a new rear sentinal to replace this one
    else if (isEmpty) newSentinals()            // add front and rear sentinals now that this list has data
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

  def clear() {
    if (isFrontSentinal) next.newFrontSentinal()
    else if (isRearSentinal) prev.newRearSentinal()
    else if (!isEmpty) {
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

  protected def newSentinals() {
    newFrontSentinal()
    newRearSentinal()
  }

  protected def newFrontSentinal() = {
    val s = makeEmpty
    s._next = self
    _prev = s
    s
  }

  protected def newRearSentinal() = {
    val s = makeEmpty
    s._prev = self
    _next = s
    s
  }

  def remove() {
    if (isFrontSentinal) {
      _next.newFrontSentinal()
      _next = null
    } else if (isRearSentinal) {
      _prev.newRearSentinal()
      _prev = null
    } else if (!isEmpty) {
      if (!_prev.isEmpty) _prev._next = _next
      if (!_next.isEmpty) _next._prev = _prev
    }
  }
}
