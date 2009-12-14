

import scala.collection.LinearSequence
import scala.collection.mutable.{ Sequence, LinearSequenceLike }
import scala.collection.generic.{ SequenceFactory, GenericTraversableTemplate }

abstract class TestSuite[CC[X] <: Sequence[X] with GenericTraversableTemplate[X, CC] with LinearSequence[X] with LinearSequenceLike[X, CC[X]]] {
  val Factory: SequenceFactory[CC]
  val testName: String
  lazy val singular = Factory(1)
  lazy val double = Factory(1, 2)
  lazy val triple = Factory(1, 2, 3)
  lazy val baseList = Factory(1, 2, 3, 4)
  lazy val doubledList = Factory(1, 2, 3, 4, 1, 2, 3, 4)
  def check[R](label: String, op: => R, expected: R): (R, Boolean) = {
    val result = op
    val correct = if (result != expected) {
      println("\t\t" + label + " == " + result + " INCORRECT, should equal" + expected)
      false
    } else true
    (result, correct)
  }
  def checkNSE(label: String, op: => Any) {
    try {
      op
      println("\t\t" + label + " should have thrown a NoSuchElementException INCORRECT")
    } catch {
      case e: NoSuchElementException => //println("\t\t" + label + " threw a NoSuchElementException CORRECT")
    }
  }
  def testEmpty() {
    println("\tTesting empty: " + testName)
    val a = Factory.empty[Int]
    check("isEmpty", a.isEmpty, true)
    check("length", a.length, 0)
    val b = Factory[Int]()
    check("a == b", a == b, true)
    checkNSE("a.head", a.head)
    checkNSE("a.tail", a.tail)
  }
  def testSingular() {
    println("\tTesting singular: " + testName)
    val a = Factory(1)
    check("a.isEmpty", a.isEmpty, false)
    check("a.length", a.length, 1)
    check("a.head", a.head, 1)
    check("a.tail.isEmpty", a.tail.isEmpty, true) 
  }
  def testInserts() {
    println("\tTesting inserts: " + testName)
    val list = Factory[Int]()
    list += 1
    check("list", list, singular)
    list += 2
    check("list" + double, list, double)
    list += 3
    check("list" + triple, list, triple)
    list += 4
    check("list" + baseList, list, baseList)
  }
  def testMakeFromSeq() {
    println("\tTesting making from a sequence: " + testName)
    check(testName + "(Nil: _*)", Factory(Nil: _*), Nil)
    check(testName + "(" + singular + ": _*)", Factory(singular: _*), singular)
    check(testName + "(" + double + ": _*)" , Factory(double: _*), double)
    check(testName + "(" + triple + ": _*)", Factory(triple: _*), triple)
    check(testName + "(" + baseList + ": _*)", Factory(baseList: _*), baseList)
  }
  def testAppendSeq() {
    println("\tTesting appending a sequence: " + testName)
    val a = Factory[Int]()
    a ++= Nil
    check("a ++= Nil => a.isEmpty", a.isEmpty, true)
    val b = Factory(1, 2, 3, 4)
    b ++= Nil
    check("b ++= Nil => b" + baseList, b, baseList)
    val c = Factory(1, 2, 3, 4)
    c ++= baseList
    check("c ++= " + baseList + " => " + baseList, c, doubledList)
    val d = Factory(1)
    d ++= List(2)
    check("d ++= List(2), d" + double, d, double)
  }
  def testAppend() {
    println("\tTesting appending a list of same type: " + testName)
    val a = Factory(1, 2, 3, 4)
    val b = Factory(1, 2, 3, 4)
    a.append(b)
    check("a.append(b) => a", a, doubledList)
    val e = Factory.empty[Int]
    val c = Factory(1, 2, 3, 4)
    e.append(c)
    check("e.append(c) => e", e, baseList)
    val e2 = Factory.empty[Int]
    val d = Factory(1, 2, 3, 4)
    d.append(e2)
    check("d.append(e2) => d", d, baseList)
  }
  def testInsert() {
    println("\tTesting insert: " + testName)
    val e = Factory.empty[Int]
    val a = Factory(1, 2, 3, 4)
    e.insert(a)
    check("e.insert(a) => e", e, baseList)
    val b = Factory(1, 2, 3, 4)
    val e2 = Factory.empty[Int]
    b.insert(e2)
    check("b.insert(e2) => b", b, baseList)
    val c = Factory(1, 2, 3, 4)
    val d = Factory(1, 2, 3, 4)
    c.insert(d)
    val r = List(1, 1, 2, 3, 4, 2, 3, 4)
    check("c.insert(d) => c", c, r)
  }
  def testAssignHead() {
    println("\tTesting assignments to head: " + testName)
    val e = Factory.empty[Int]
    e.head = 1
    check("e.head = 1 => e.head", e.head, 1)
    check("e.length", e.length, 1)
    check("e.tail.isEmpty", e.tail.isEmpty, true)
    val a = Factory(1, 2)
    a.head = 3
    check("a.head = 3 => a.head", a.head, 3)
    check("a.length", a.length, 2)
    check("a.tail.head", a.tail.head, 2)
  }
  def testAssignTail() {
    println("\tTesting assignments to tail: " + testName)
    val e = Factory.empty[Int]
    checkNSE("e.tail = Factory(1, 2, 3)", e.tail = Factory(1, 2, 3))
    val a = Factory(1)
    val b = Factory(2)
    a.tail = b
    check("a.tail = b => a.tail", a.tail, b)
    check("a.length", a.length, 2)
    a.tail = Factory.empty[Int]
    check("a.tail.isEmpty", a.tail.isEmpty, true)
    check("a.length", a.length, 1)
  }
  def testClear() {
    println("\tTesting clear(): " + testName)
    val e = Factory.empty[Int]
    e.clear()
    check("e.isEmpty", e.isEmpty, true)
    val a = Factory(1, 2, 3, 4)
    val b = a.tail
    a.clear()
    check("a.isEmpty", a.isEmpty, true)
    check("b.isEmpty", b.isEmpty, false)
    check("b.length", b.length, 3)
    check("b.head", b.head, 2)
  }
  def testGet() {
    println("\tTesting get(): " + testName)
    val list = Factory(1, 2, 3, 4)
    check("list.get(0)", list.get(0), Some(1))
    check("list.get(2)", list.get(2), Some(3))
    check("list.get(5)", list.get(5), None)
    val empty = Factory.empty[Int]
    check("empty.get(0)", empty.get(0), None)
    val single = Factory(1)
    check("single.get(0)", single.get(0), Some(1))
    check("single.get(1)", single.get(1), None)
  }
  def testApply() {
    println("\tTesting apply()")
    val empty = Factory.empty[Int]
    checkNSE("empty(0)", empty(0))
    val single = Factory(1)
    check("single(0)", single(0), 1)
    checkNSE("single(1)", single(1))
    val list = Factory(1, 2, 3, 4)
    check("list(0)", list(0), 1)
    check("list(2)", list(2), 3)
    check("list(2)", list(3), 4)
    checkNSE("list(5)", list(5))
  }
  def testUpdate() {
    println("\tTesting update: " + testName)
    val empty = Factory.empty[Int]
    checkNSE("empty(0) = 1", empty(0) = 1)
    val single = Factory(1)
    single(0) = 2
    check("single(0) = 2, single(0)", single(0), 2)
    checkNSE("single(1) = 2", single(1) = 2)
    val list = Factory(1, 2, 3, 4)
    list(0) = 5
    check("list(0) = 5, list(0)", list(0), 5)
    list(2) = 7
    check("list(2) = 7, list(2)", list(2), 7)
    checkNSE("list(5) = 8", list(5) = 8)
  }
  def run() {
    println("Testing: " + testName)
    testEmpty()
    testSingular()
    testInserts()
    testMakeFromSeq()
    testAppendSeq()
    testAppend()
    testInsert()
    testAssignHead()
    testAssignTail()
    testClear()
    testGet()
    testApply()
    testUpdate()
    println("Done testing: " + testName)
  }
  def main(args: Array[String]) {
    run()
  }
}

import scala.collection.mutable.{ LinkedListLike, LinkedList }

abstract class LinkedListTestSuite[CC[X] <: Sequence[X] with GenericTraversableTemplate[X, CC] with LinearSequence[X] with LinkedListLike[X, CC[X]]] extends TestSuite[CC] {
  // no special tests yet
}

object LinkedListTest extends LinkedListTestSuite[LinkedList] {
  val Factory = LinkedList
  val testName = "LinkedList"
}

import scala.collection.mutable.DoubleLinkedListLike
abstract class DoubleLinkedListTestSuite[CC[X] <: Sequence[X] with GenericTraversableTemplate[X, CC] with LinearSequence[X] with DoubleLinkedListLike[X, CC[X]]] 
               extends TestSuite[CC] {
  def getLastNode[A](node: CC[A]): CC[A] = if (node.tail.isEmpty) node else getLastNode(node.tail)
  def getFirstNode[A](node: CC[A]): CC[A] = if (node.prev.isEmpty) node else getFirstNode(node.prev)
  def checkSentinals(label: String, list: CC[_]) {
    val first = getFirstNode(list)
    if (first.isEmpty) {
      // first is empty, it should not have any sentinals
      checkNSE(label + " check first.tail", first.tail)
      checkNSE(label + " check first.prev", first.prev)
      checkNSE(label + " check first.next", first.next)
    } else {
      // not empty, should have sentinals
      // previous should be a front sentinals
      check(label + " check first.prev.isFrontSentinal", first.prev.isFrontSentinal, true)
      check(label + " check first.prev.isRearSentinal", first.prev.isRearSentinal, false)
      check(label + " check first.prev.isSentinal", first.prev.isSentinal, true)
      check(label + " check first.prev.isEmpty", first.prev.isEmpty, true)
      check(label + " check first.prev.tail eq " + label, first.prev.tail eq first, true)
      checkNSE(label + " check first.prev.prev", first.prev.prev)
      // next of last node should be a rear sentinal
      val last = getLastNode(first)
      check(label + " check last.next.isFrontSentinal", last.next.isFrontSentinal, false)
      check(label + " check last.next.isRearSentinal", last.next.isRearSentinal, true)
      check(label + " check last.next.isSentinal", last.next.isSentinal, true)
      check(label + " check last.next.isEmpty", last.next.isEmpty, true)
      check(label + " check last.next.prev eq " + label, last.next.prev eq last, true)
    }
  }
  override def check[R](label: String, op: => R, expected: R): (R, Boolean) = {
    val (result, correct) = super.check(label, op, expected)
    if (correct) {
      result match {
	case list: CC[_] => checkSentinals(label, list)
	case _ => // do nothing
      }
    }
    (result, correct)
  }

  override def testAppend() {
    super.testAppend()
    // test appends involving nodes from the middles of list to ensure the lists are properly broken apart
    // and are all still valid
  }

  override def testInsert() {
    super.testInsert()
    // test insert on front sentinal
    val l1 = Factory(1)
    val l2 = Factory(2)
    check("l1.prev.insert(l2)", l1.prev.insert(l2), Factory(2, 1))
    // test insert on rear sentinal
    val l3 = Factory(1)
    val l4 = Factory(2)
    check("l3.next.insert(l4)", l3.next.insert(l4), Factory(1, 2))
    // insert of middle of one list into the middle of another list
    val l5 = Factory(1, 2, 3, 4)
    val l5tail = l5.tail
    val l6 = Factory(5, 6, 7, 8)
    val l6tail = l6.tail
    l5tail.insert(l6tail)
    check("l5", l5, Factory(1, 2, 6, 7, 8, 3, 4))
    check("l5tail", l5tail, Factory(2, 6, 7, 8, 3, 4))
    check("l6tail", l6tail, Factory(6, 7, 8, 3, 4)) //hmmm, this brings about an interesting issue regarding equality and double-linked lists
    check("l6", l6, Factory(5)) // the front of L6 has now been broken off from what used to be the rest of L6
  }
  
  def testRemove() {
    println("\tTesting remove: " + testName)
    // remove only node
    val t1 = Factory(1)
    t1.remove()
    check("t1.isEmptyList", t1.isListEmpty, true)
    // remove central node from list with several items
    val t2 = Factory(1, 2, 3, 4, 5)
    val t2removed = t2.tail.tail
    t2removed.remove()
    check("t2removed.head", t2removed.head, 3)
    check("t2removed.length", t2removed.length, 1)
    check("t2", t2, Factory(1, 2, 4, 5))
    // remove last node from list with several items
    val t3 = Factory(1, 2, 3, 4)
    val t3removed = t3.tail.tail.tail
    t3removed.remove()
    check("t3removed.head", t3removed.head, 4)
    check("t3removed.length", t3removed.length, 1)
    check("t3", t3, Factory(1, 2, 3))
    // remove first node from list with several items
    val t5 = Factory(1, 2, 3, 4)
    val t5sentinal = t5.prev
    t5.remove()
    check("t5.head", t5.head, 1)
    check("t5.length", t5.length, 1)
    check("t5sentinal.tail", t5sentinal.tail, Factory(2, 3, 4))
    // remove empty node
    val t6 = Factory()
    t6.remove()
    check("t6", t6, Factory())
    // remove front sentinal node
    val t7 = Factory(1, 2, 3, 4)
    val t7frontSentinal = t7.prev
    t7frontSentinal.remove()
    check("t7", t7, Factory(1, 2, 3, 4))
    check("t7frontSentinal.isEmpty", t7frontSentinal.isEmpty, true)
    check("t7frontSentinal.isSentinal", t7frontSentinal.isSentinal, false)
    // remove rear sentinal node
    val t8 = Factory(1, 2, 3, 4)
    val t8rearSentinal = t8.tail.tail.tail.tail
    t8rearSentinal.remove()
    check("t8", t8, Factory(1, 2, 3, 4))
    check("t8rearSentinal.isEmpty", t8rearSentinal.isEmpty, true)
    check("t8rearSentinal.isSentinal", t8rearSentinal.isSentinal, false)
  }
  // TODO: test sentinal node equality

  def testAssignPrev() {
    println("\tTODO: write checks for assigning prev")
  }

  override def testAssignTail() {
    super.testAssignTail()
    print("\tTODO: write checks that ensure assigning to tail properly unlinks tails from the list")
  }
  override def run() {
    super.run()
    testRemove()
    testAssignPrev()
  }
}

import scala.collection.mutable.DoubleLinkedList
object DoubleLinkedListTest extends TestSuite[DoubleLinkedList] {
  val Factory = DoubleLinkedList
  val testName = "DoubleLinkedList"
}

import scala.collection.mutable.Queue
object QueueTest extends TestSuite[Queue] {
  val Factory = Queue
  val testName = "Queue"
}

object Test {
  def main(args: Array[String]) {
    LinkedListTest.run()
    DoubleLinkedListTest.run()
    QueueTest.run()
  }
}
