/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import scala.collection.generic
import scala.collection


/** A subtrait of <code>collection.Sequence</code> which represents sequences
 *  that cannot be mutated.
 *
 *  @since 2.8
 */
trait LinearSequence[A] extends Sequence[A] 
                           with scala.collection.LinearSequence[A] 
                           with generic.GenericTraversableTemplate[A, LinearSequence]
                           with LinearSequenceLike[A, LinearSequence[A]] {
  override def companion: generic.GenericCompanion[LinearSequence] = LinearSequence
}

object LinearSequence extends generic.SequenceFactory[LinearSequence] {
  implicit def builderFactory[A]: generic.BuilderFactory[A, LinearSequence[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, LinearSequence[A]] = LinkedList.newBuilder[A]
}
