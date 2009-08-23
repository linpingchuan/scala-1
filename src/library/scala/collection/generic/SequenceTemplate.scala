/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import mutable.{ListBuffer, HashMap}

// import immutable.{List, Nil, ::}
import generic._

/** Class <code>Sequence[A]</code> represents sequences of elements
 *  of type <code>A</code>.
 *  It adds the following methods to class Iterable:
 *   `length`, `lengthCompare`, `apply`, `isDefinedAt`, `segmentLength`, `prefixLength`,
 *   `indexWhere`, `indexOf`, `lastIndexWhere`, `lastIndexOf`, `reverse`, `reverseIterator`,
 *   `startsWith`, `endsWith`, `indexOfSeq`, , `zip`, `zipAll`, `zipWithIndex`.
 * 
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait SequenceTemplate[+A, +This <: IterableTemplate[A, This] with Sequence[A]] extends IterableTemplate[A, This] { self =>
  
  import Traversable.breaks._

  /** Returns the length of the sequence.
   *
   *  @return the sequence length.
   */
  def length: Int

  /** Returns the elements at position `idx`
   */
  def apply(idx: Int): A

  /** Result of comparing <code>length</code> with operand <code>len</code>.
   *  returns <code>x</code> where
   *  <code>x &lt; 0</code>    iff    <code>this.length &lt; len</code>
   *  <code>x == 0</code>   iff    <code>this.length == len</code>
   *  <code>x &gt; 0</code>    iff    <code>this.length &gt; len</code>.
   *
   *  The method as implemented here does not call length directly; its running time
   *  is O(length min len) instead of O(length). The method should be overwritten
   *  if computing length is cheap.
   */
  def lengthCompare(len: Int): Int = {
    var i = 0
    breakable {
      for (_ <- this) {
        i += 1
        if (i > len) break
      }
    }
    i - len
  }

  /** Should always be <code>length</code> */
  override def size = length

  /** Is this partial function defined for the index <code>x</code>?
   */
  def isDefinedAt(x: Int): Boolean = (x >= 0) && (x < length)

  /** Returns an iterable formed from this iterable and another iterable
   *  by combining corresponding elements in pairs.
   *  If one of the two iterables is longer than the other, its remaining elements are ignored.
   *  @param   that  The iterable providing the second half of each result pair
   */
  def zip[A1 >: A, B, That](that: Sequence[B])(implicit bf: BuilderFactory[(A1, B), That, This]): That = {
    val b = bf(thisCollection)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    b.result
  }

  /** Returns a iterable formed from this iterable and the specified iterable
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *
   *  @param that     iterable <code>that</code> may have a different length
   *                  as the self iterable.
   *  @param thisElem element <code>thisElem</code> is used to fill up the
   *                  resulting iterable if the self iterable is shorter than
   *                  <code>that</code>
   *  @param thatElem element <code>thatElem</code> is used to fill up the
   *                  resulting iterable if <code>that</code> is shorter than
   *                  the self iterable
   *  @return         <code>Sequence((a<sub>0</sub>,b<sub>0</sub>), ...,
   *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
   *                  ..., {elem,b<sub>m</sub>})</code>
   *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
   *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
   *                  invoked where <code>m &gt; n</code>.
   *  
   */
  def zipAll[B, A1 >: A, That](that: Sequence[B], thisElem: A1, thatElem: B)(implicit bf: BuilderFactory[(A1, B), That, This]): That = {  
    val b = bf(thisCollection)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    while (these.hasNext)
      b += ((these.next, thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next))
    b.result
  }

  /** Zips this iterable with its indices (startiong from 0). 
   */
  def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, This]): That = {
    val b = bf(thisCollection)
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i +=1 
    }
    b.result
  }

  /** Returns length of longest segment starting from a start index `from`
   *  such that every element of the segment satisfies predicate `p`.
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  def segmentLength(p: A => Boolean, from: Int): Int = {
    var result = 0
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= from && !p(x)) { result = i - from; break }
        else i += 1
      }
    }
    result
  }

  /** Returns length of longest prefix of this seqence
   *  such that every element of the prefix satisfies predicate `p`.
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   */
  def prefixLength(p: A => Boolean) = segmentLength(p, 0)

  /** Returns index of the first element satisfying a predicate, or -1, if none exists.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Returns index of the first element starting from a start index
   *  satisying a predicate, or -1, if none exists.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var result = -1
    var i = from
    breakable {
      for (x <- this) {
        if (i >= from && p(x)) { result = i; break }
        else i += 1
      }
    }
    result
  }

  /** Returns index of the first element satisying a predicate, or -1. */
  @deprecated("Use `indexWhere' instead")
  def findIndexOf(p: A => Boolean): Int = indexWhere(p)

  /** Returns the index of the first occurence of the specified
   *  object in this iterable object.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  elem  element to search for.
   *  @return the index in this sequence of the first occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Returns the index of the first occurence of the specified
   *  object in this iterable object,  starting from a start index, or
   *  -1, if none exists.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  elem  element to search for.
   */
  def indexOf[B >: A](elem: B, from: Int): Int = indexWhere(elem ==, from)

 /** Returns the index of the last occurence of the specified element
   *  in this sequence, or -1 if the sequence does not contain this element.
   *
   *  @param  elem   element to search for.
   *  @return the index in this sequence of the last occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def lastIndexOf[B >: A](elem: B): Int = lastIndexWhere(elem ==)

  /** Returns the index of the last
    *  occurence of the specified element in this sequence
    *  before or at a given end index,
    *  or -1 if the sequence does not contain this element.
    *
    *  @param  elem   element to search for.
    *  @param  end    the end index
    */
  def lastIndexOf[B >: A](elem: B, end: Int): Int = lastIndexWhere(elem ==, end)

  /** Returns index of the last element satisying a predicate, or -1, if none exists.
   *
   *  @param  p the predicate
   *  @return   the index of the last element satisfying <code>p</code>,
   *            or -1 if such an element does not exist
   */
  def lastIndexWhere(p: A => Boolean): Int = lastIndexWhere(p, length - 1)

  /** Returns index of the last element not exceeding a given end index
   *  and satisying a predicate, or -1 if none exists.
   *
   *  @param  end the end index
   *  @param  p the predicate
   */
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = length - 1
    val it = reverseIterator
    while (it.hasNext && { val elem = it.next; (i > end || !p(elem)) }) i -= 1
    i
  }

  /** A sequence of type <code>C</code> consisting of all elements of 
   *  this sequence in reverse order.
   *  @note  the operation is implemented by building a new sequence
   *         from <code>this(length - 1), ..., this(0)</code>
   *  If random access is inefficient for the given sequence implementation, 
   *  this operation should be overridden.
   */
  def reverse: This = {
    var xs: List[A] = List()
    for (x <- this) 
      xs = x :: xs
    val b = newBuilder
    for (x <- xs)
      b += x
    b.result
  }

  /** The elements of this sequence in reversed order
   */
  def reverseIterator: Iterator[A] = reverse.iterator

  @deprecated("use `reverseIterator' instead")
  def reversedElements = reverseIterator

  /**
   * Checks whether the argument sequence is contained at the 
   * specified index within the receiver object. 
   * 
   * If the both the receiver object, <code>this</code> and 
   * the argument, <code>that</code> are infinite sequences 
   * this method may not terminate.
   *
   * @return true if <code>that</code> is contained in 
   * <code>this</code>, at the specified index, otherwise false 
   *
   * @see String.startsWith
   */
  def startsWith[B](that: Sequence[B], offset: Int): Boolean = {  
    val i = this.iterator drop offset
    val j = that.iterator
    while (j.hasNext && i.hasNext)
      if (i.next != j.next)
        return false

    !j.hasNext
  }

  /**
   * Check whether the receiver object starts with the argument sequence.
   *
   * @return true if <code>that</code> is a prefix of <code>this</code>,
   * otherwise false 
   */
  def startsWith[B](that: Sequence[B]): Boolean = startsWith(that, 0)

  /** @return true if this sequence end with that sequence 
   *  @see String.endsWith 
   */
  def endsWith[B](that: Sequence[B]): Boolean = {
    val i = this.iterator.drop(length - that.length)
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (i.next != j.next)
        return false
    
    !j.hasNext
  }

  /** @return -1 if <code>that</code> not contained in this, otherwise the
   *  first index where <code>that</code> is contained.
   */
  def indexOfSeq[B >: A](that: Sequence[B]): Int = indexOfSeq(that, 0)
  
  def indexOfSeq[B >: A](that: Sequence[B], fromIndex: Int): Int = 
    if (thisCollection.hasDefiniteSize && that.hasDefiniteSize)
      indexOf_KMP(thisCollection, 0, length, that, 0, that.length, fromIndex)
    else {
      var i = fromIndex
      var s: Sequence[A] = thisCollection drop i
      while (!s.isEmpty) {
        if (s startsWith that)
          return i
        
        i += 1
        s = s.tail
      }
      -1
    }
      
  /** @return -1 if <code>that</code> not contained in this, otherwise the
  *  last index where <code>that</code> is contained.
  *  @note may not terminate for infinite-sized collections.
  */
  def lastIndexOfSeq[B >: A](that: Sequence[B]): Int = lastIndexOfSeq(that, that.length)

  // since there's no way to find the last index in an infinite sequence,
  // we just document it may not terminate and assume it will.
  def lastIndexOfSeq[B >: A](that: Sequence[B], fromIndex: Int): Int = 
    lastIndexOf_KMP(thisCollection, 0, length, that, 0, that.length, fromIndex)

  /** Tests if the given value <code>elem</code> is a member of this 
   *  sequence.
   *
   *  @param elem element whose membership has to be tested.
   *  @return     <code>true</code> iff there is an element of this sequence
   *              which is equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists (_ == elem)

  /** <p>
   *    Computes the multiset union of this sequence and the given sequence
   *    <code>that</code>. For example:
   *  </p><pre>
   *    <b>val</b> xs = List(1, 1, 2)
   *    <b>val</b> ys = List(1, 2, 2, 3)
   *    println(xs union ys)  // prints "List(1, 1, 2, 1, 2, 2, 3)"
   *    println(ys union xs)  // prints "List(1, 2, 2, 3, 1, 1, 2)"
   *  </pre>
   *
   *  @param that the sequence of elements to add to the sequence.
   *  @return     a sequence containing the elements of this
   *              sequence and those of the given sequence <code>that</code>.
   */
  def union[B >: A, That](that: Sequence[B])(implicit bf: BuilderFactory[B, That, This]): That = 
    thisCollection ++ that

  /** <p>
   *    Computes the multiset difference between this sequence and the
   *    given sequence <code>that</code>. If an element appears more
   *    than once in both sequences, the difference contains <i>m</i> copies
   *    of that element, where <i>m</i> is the difference between the
   *    number of times the element appears in this sequence and the number
   *    of times it appears in <code>that</code>. For example:
   *  </p><pre>
   *    <b>val</b> xs = List(1, 1, 2)
   *    <b>val</b> ys = List(1, 2, 2, 3)
   *    println(xs diff ys)  // prints "List(1)"
   *    println(xs -- ys)    // prints "List()"
   *  </pre>
   *
   *  @param that the sequence of elements to remove from this sequence.
   *  @return     the sequence of elements contained only in this sequence plus
   *              <i>m</i> copies of each element present in both sequences,
   *              where <i>m</i> is defined as above.
   */
  def diff[B >: A, That](that: Sequence[B]): This = {
    val occ = occCounts(that)
    val b = newBuilder
    for (x <- this)
      if (occ(x) == 0) b += x
      else occ(x) -= 1 
    b.result
  }

  /** <p>
   *    Computes the multiset intersection between this sequence and the
   *    given sequence <code>that</code>; the intersection contains <i>m</i>
   *    copies of an element contained in both sequences, where <i>m</i> is
   *    the smaller of the number of times the element appears in this
   *    sequence or in <code>that</code>. For example:
   *  </p><pre>
   *    <b>val</b> xs = List(1, 1, 2)
   *    <b>val</b> ys = List(3, 2, 2, 1)
   *    println(xs intersect ys)  // prints "List(1, 2)"
   *    println(ys intersect xs)  // prints "List(2, 1)"
   *  </pre>
   *
   *  @param that the sequence to intersect.
   *  @return     the sequence of elements contained both in this sequence and
   *              in the given sequence <code>that</code>.
   */
  def intersect[B >: A, That](that: Sequence[B]): This = {
    val occ = occCounts(that)
    val b = newBuilder
    for (x <- this)
      if (occ(x) > 0) {
        b += x
        occ(x) -= 1
      }
    b.result
  }

  private def occCounts[B](seq: Sequence[B]): mutable.Map[B, Int] = {
    val occ = 
      if (seq.isEmpty || seq.head.isInstanceOf[Unhashable]) 
        new mutable.ListMap[B, Int] { override def default(k: B) = 0 }
      else         
        new mutable.HashMap[B, Int] { override def default(k: B) = 0 }
    for (y <- seq) occ(y) += 1
    occ
  }

  /** Builds a new sequence from this sequence in which any duplicates (wrt to ==) removed.
   *  Among duplicate elements, only the first one is retained in the result sequence
   */
  def removeDuplicates: This = {
    val b = newBuilder
    var seen = Set[A]()
    for (x <- this) {
      if (!(seen contains x)) {
        b += x
        seen = (seen + x)
      }
    }
    b.result
  }

  /** A new sequence, consisting of all elements of current sequence 
   *  except that `replaced` elements starting from `from` are replaced
   *  by `patch`.
   */
  def patch[B >: A, That](from: Int, patch: Sequence[B], replaced: Int)(implicit bf: BuilderFactory[B, That, This]): That = {
    val b = bf(thisCollection)
    val (prefix, rest) = this.splitAt(from)
    b ++= prefix
    b ++= patch
    b ++= rest drop replaced
    b.result
  }

  /** Returns a new sequence of given length containing the elements of this sequence followed by zero
   *  or more occurrences of given elements. 
   */
  def padTo[B >: A, That](len: Int, elem: B)(implicit bf: BuilderFactory[B, That, This]): That = {
    val b = bf(thisCollection)
    var diff = len - length
    b ++= thisCollection
    while (diff > 0) {
      b += elem
      diff -=1 
    }
    b.result
  }

  /**
   *  Overridden for efficiency.
   *
   *  @return  the sequence itself
   */
  override def toSequence: Sequence[A] = thisCollection

  def indices: Range = 0 until length

  override def view = new SequenceView[A, This] {
    protected lazy val underlying = self.thisCollection
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override def view(from: Int, until: Int) = view.slice(from, until)
    
  // KMP implementation by paulp, based on the undoubtedly reliable wikipedia entry
  private def KMP[B](S: Seq[B], W: Seq[B]): Option[Int] = {
    // trivial cases
    if (W.isEmpty) return Some(0)
    else if (W drop 1 isEmpty) return (S indexOf W(0)) match {
      case -1 => None
      case x  => Some(x)
    }
    
    val T: Array[Int] = {
      val arr = new Array[Int](W.length)
      var pos = 2
      var cnd = 0
      arr(0) = -1
      arr(1) = 0
      while (pos < W.length) {
        if (W(pos - 1) == W(cnd)) {
          arr(pos) = cnd + 1
          pos += 1
          cnd += 1
        }
        else if (cnd > 0) {
          cnd = arr(cnd)
        }
        else {
          arr(pos) = 0
          pos += 1
        }
      }
      arr
    }
    
    var m, i = 0
    def mi = m + i
    
    while (mi < S.length) {
      if (W(i) == S(mi)) {
        i += 1
        if (i == W.length)
          return Some(m)
      }
      else {
        m = mi - T(i)
        if (i > 0)
          i = T(i)
      }
    }
    None
  }
  private def indexOf_KMP[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int): Int =
      KMP(source.slice(sourceOffset, sourceCount) drop fromIndex, target.slice(targetOffset, targetCount)) match {
        case None     => -1
        case Some(x)  => x + fromIndex
      }

  private def lastIndexOf_KMP[B](
    source: Seq[B], sourceOffset: Int, sourceCount: Int,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Int): Int = {
      val src = (source.slice(sourceOffset, sourceCount) take fromIndex).reverse
      val tgt = target.slice(targetOffset, targetCount).reverse

      KMP(src, tgt) match {
        case None     => -1
        case Some(x)  => (src.length - tgt.length - x) + sourceOffset
      }
    }

  override def equals(that: Any): Boolean = that match {
    case that: Sequence[_]  => this sameElements that
    case _                  => false
  }

  /** Need to override string, so that it's not the Function1's string that gets mixed in.
   */
  override def toString = super[IterableTemplate].toString

  /** Sort the sequence according to the comparison function
   *  <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *  which should be true iff <code>e1</code> is smaller than
   *  <code>e2</code>.
   *  The sort is stable. That is elements that are equal wrt `lt` appear in the
   *  same order in the sorted sequence as in the original.
   *
   *  @param lt the comparison function
   *  @return   a sequence sorted according to the comparison function
   *            <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sortWith((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   */
  def sortWith(lt: (A, A) => Boolean): This = {
    val arr = toArray
    import java.util.{Arrays, Comparator}
    Arrays.sort(arr, new Comparator[A] {
      override def compare(a: A, b: A) =
        if (lt(a, b)) -1 else if (lt(b, a)) 1 else 0
    })
    val b = newBuilder
    for (x <- arr) b += x
    b.result
  }

  /** Returns index of the last element satisying a predicate, or -1. */
  @deprecated("use `lastIndexWhere' instead")
  def findLastIndexOf(p: A => Boolean): Int = lastIndexWhere(p)
  
  /** A sub-sequence starting at index <code>from</code> 
    *  and extending up to the length of the current sequence
    *
    *  @param from   The index of the first element of the slice
    *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
    */
  @deprecated("use `drop' instead")
  def slice(from: Int): Sequence[A] = slice(from, length)

  @deprecated("Should be replaced by <code>(s1, s2) forall { case (x, y) => f(x, y) }</code>")
  def equalsWith[B](that: Sequence[B])(f: (A,B) => Boolean): Boolean = {
    val i = this.iterator
    val j = that.iterator
    while (i.hasNext && j.hasNext)
      if (!f(i.next, j.next))
        return false
    
    !i.hasNext && !j.hasNext
  }

  /** Is <code>that</code> a slice in this? */
  @deprecated("Should be replaced by <code>indexOfSeq(that) != -1</code>")
  def containsSlice[B](that: Sequence[B]): Boolean = indexOfSeq(that) != -1 

 /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
   */
  @deprecated("use `view' instead")
  override def projection = view
}

