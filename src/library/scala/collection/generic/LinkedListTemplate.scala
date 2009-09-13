/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._
import annotation.tailrec

/** This extensible class may be used as a basis for implementing linked
 *  list. Type variable <code>A</code> refers to the element type of the
 *  list, type variable <code>This</code> is used to model self types of
 *  linked lists.
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait LinkedListTemplate[A, This >: Null <: Sequence[A] with LinkedListTemplate[A, This]] extends SequenceTemplate[A, This] {
  self: This =>
  
  private var elem: A = _
  private var next: This = null

  protected def makeEmpty: This
  protected def makeFromSequence(seq: Sequence[A]): This
  //protected def newBuilder: Builder[A, This]

  override def isEmpty = next eq null

  override def length: Int = {
    @tailrec
    def loop(x: This, cnt: Int): Int = if (x.isEmpty) cnt else loop(x.next, cnt + 1)
    loop(self, 0)
  }

  /** add <code>e</code> to the front of the list */
  protected def prependElem(e: A): This = {
    val n = makeEmpty
    n.next = next
    n.elem = elem
    elem = e
    next = n
    self
  }

  def +=(e: A): This = prependElem(e)

  /** add <code>e</code> to the back of the list */
  protected def appendElem(e: A): This = {
    val n = makeEmpty
    val terminal = terminalNode
    terminal.elem = e
    terminal.next = n
    self
  }

  //TODO: this must be wrong...
  def append(e: A): Unit = appendElem(e)

  protected def prependNodes(nodes: This): This = {
    if (!nodes.isEmpty) {
      val thisHead = head
      val thisTail = tail
      val otherHead = nodes.head
      val otherTail = nodes.tail
      val otherLastElement = nodes.lastElementNode
      elem = otherHead
      next = otherTail
      nodes.elem = thisHead
      nodes.next = thisTail
      otherLastElement.next = nodes
    }
    self
  }

  def ++=(elems: Sequence[A]): This = {
    val list = makeFromSequence(elems)
    prependNodes(list)
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

  override def head: A    = if (!isEmpty) elem else throw new NoSuchElementException("head of an empty list")
  override def tail: This = if (!isEmpty) next else throw new NoSuchElementException("list has no elements")

  override def drop(n: Int): This = {
    //TODO: this is essentially the same as in immutable.List
    @tailrec
    def loop(xs: This, at: Int): This = if (xs.isEmpty || at == n) xs else loop(xs.tail, at + 1)
    loop(self, 0)
  }
  private def atLocation[T](n: Int)(f: This => T) = {
    val loc = drop(n)
    if (!loc.isEmpty) f(loc)
    else throw new IndexOutOfBoundsException(n.toString)
  }

  override def apply(n: Int): A   = atLocation(n)(_.elem)
  def update(n: Int, x: A): Unit  = atLocation(n)(_.elem = x)

  def get(n: Int): Option[A] = {
    //TODO: this is probably the same as in immutable.List or at least could be
    val loc = drop(n)
    if (loc.isEmpty) None else Some(loc.elem)
  }

  override def iterator: Iterator[A] = new Iterator[A] {
    var elems = self
    def hasNext = elems.isEmpty
    def next = {
      val res = elems.head
      elems = elems.tail
      res
    }
  }

  override def foreach[B](f: A => B) {
    @tailrec
    def loop(xs: This): Unit = if (!xs.isEmpty) {
      f(xs.head)
      loop(xs.tail)
    }
    loop(self)
  }
}
