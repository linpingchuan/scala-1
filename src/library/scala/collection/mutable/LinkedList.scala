/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.generic.{ GenericTraversableTemplate, SequenceFactory, GenericCompanion, BuilderFactory }
import scala.{ collection => col }

/** This class implements single linked lists where both the head (<code>elem</code>)
 *  and the tail (<code>next</code>) are mutable.
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 */
@serializable
class LinkedList[A] extends Sequence[A]
                       with col.LinearSequence[A]
                       with GenericTraversableTemplate[A, LinkedList]
                       with LinkedListLike[A, LinkedList[A]]  {
  def this(v: A) {
   this()
   elem = v
   next = makeEmpty
  }
  protected def makeEmpty = new LinkedList[A]()

  protected def makeFromTraversable(seq: col.Traversable[A]) = {
    val builder = LinkedList.newBuilder[A]
    builder ++= seq
    builder.result()
  }
  override def companion: GenericCompanion[LinkedList] = LinkedList  //TODO: should this be a lower class, such as SequenceFactory?
}

object LinkedList extends SequenceFactory[LinkedList] {
  implicit def builderFactory[A]: BuilderFactory[A, LinkedList[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A] = new Builder[A, LinkedList[A]] {
    var front: LinkedList[A] = _
    var back: LinkedList[A] = _
    clear() // initializes front and back
    def +=(elem: A): this.type = {
      if (front eq null) {
	front = new LinkedList(elem)
	back = front
      } else {
	val n = new LinkedList(elem)
	back.append(n)
	back = n
      }
      this
    }
    def clear() {
      front = null
      back = null
    }
    def result() = front
  }
}
