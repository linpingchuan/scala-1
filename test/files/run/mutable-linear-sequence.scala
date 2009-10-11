

import scala.collection.LinearSequence
import scala.collection.mutable.{ Sequence, LinearSequenceLike }
import scala.collection.generic.{ SequenceFactory, GenericTraversableTemplate }

abstract class TestSuite[CC[X] <: Sequence[X] with GenericTraversableTemplate[X, CC] with LinearSequence[X] with LinearSequenceLike[X, CC[X]]] {
  val Factory: SequenceFactory[CC]
  val testName: String
  val singular = List(1)
  val double = List(1, 2)
  val triple = List(1, 2, 3)
  val baseList = List(1, 2, 3, 4)
  val doubledList = List(1, 2, 3, 4, 1, 2, 3, 4)
  def check[R](label: String, op: => R, expected: R) {
    val result = op
    val correctness = if (result == expected) "CORRECT" else "INCORRECT, should equal " + expected
    println("\t\t" + label + " " + result + " " + correctness)
  }
  def testEmpty() {
    println("\tTesting empty: " + testName)
    val a = Factory.empty[Int]
    check("isEmpty =", a.isEmpty, true)
    check("length =", a.length, 0)
    val b = Factory[Int]()
    check("a == b =", a == b, true)
    try {
      a.head
      println("\t\ta.head should have thrown a NoSuchElementException")
    } catch {
      case e: NoSuchElementException => "\t\ta.head threw " + e + " CORRECT"
      case e: Exception => "\t\ta.head threw " + e + " INCORRECT, should be NoSuchElementException"
    }
    try {
      a.tail
      println("\t\ta.tail should have thrown a NoSuchElementException")
    } catch {
      case e: NoSuchElementException => "\t\ta.tail threw " + e + " CORRECT"
      case e: Exception => "\t\ta.tail threw " + e + " INCORRECT"
    }
  }
  def testSingular() {
    println("\tTesting singular: " + testName)
    val a = Factory(1)
    check("isEmpty =", a.isEmpty, false)
    check("length =", a.length, 1)
    check("head =", a.head, 1)
    check("tail.isEmpty =", a.tail.isEmpty, true) 
  }
  def testInserts() {
    println("\tTesting inserts: " + testName)
    val list = Factory[Int]()
    list += 1
    check("list == List(1) =", list, singular)
    list += 2
    check("list == " + double, list, double)
    list += 3
    check("list == " + triple, list, triple)
    list += 4
    check("list == " + baseList, list, baseList)
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
    check("b ++= Nil => b == " + baseList, b, baseList)
    val c = Factory(1, 2, 3, 4)
    c ++= baseList
    check("c ++= " + baseList + " => c == " + baseList, c, doubledList)
    val d = Factory(1)
    d ++= List(2)
    check("d ++= List(2), d == " + double, d, double)
  }
  def testAppend() {
    println("\tTesting appending a list of same type: " + testName)
    val a = Factory(1, 2, 3, 4)
    val b = Factory(1, 2, 3, 4)
    a.append(b)
    check("a.append(b) => a == " + doubledList, a, doubledList)
    val e = Factory.empty[Int]
    val c = Factory(1, 2, 3, 4)
    e.append(c)
    check("e.append(c) => e == " + baseList, e, baseList)
    val e2 = Factory.empty[Int]
    val d = Factory(1, 2, 3, 4)
    d.append(e2)
    check("d.append(e2) => d == " + baseList, d, baseList)
  }
  def testInsert() {
    println("\tTesting insert: " + testName)
    val e = Factory.empty[Int]
    val a = Factory(1, 2, 3, 4)
    e.insert(a)
    check("e.insert(a) => e == " + baseList, e, baseList)
    val b = Factory(1, 2, 3, 4)
    val e2 = Factory.empty[Int]
    b.insert(e2)
    check("b.insert(e2) => b == " + baseList, b, baseList)
    val c = Factory(1, 2, 3, 4)
    val d = Factory(1, 2, 3, 4)
    c.insert(d)
    val r = List(1, 1, 2, 3, 4, 2, 3, 4)
    check("c.insert(d) => c == " + r, c, r)
  }
  def testAssignHead() {
    println("\tTesting assignments to head: " + testName)
    val e = Factory.empty[Int]
    e.head = 1
    check("e.head = 1 => e.head == 1", e.head, 1)
    check("e.length == 1", e.length, 1)
    check("e.tail.isEmpty == true", e.tail.isEmpty, true)
    val a = Factory(1, 2)
    a.head = 3
    check("a.head = 3 => a.head == 3", a.head, 3)
    check("a.length == 2", a.length, 2)
    check("a.tail.head == 2", a.tail.head, 2)
  }
  def testAssignTail() {
    println("\tTesting assignments to tail: " + testName)
    val e = Factory.empty[Int]
    try {
      e.tail = Factory(1, 2, 3)
      println("\t\tAssignment to tail of empty " + testName + " should have thrown a NoSuchElementException")
    } catch {
      case e: NoSuchElementException => println("\t\tcaught NoSuchElementException CORRECT")
      case e: Exception => println("\t\tcaught " + e + " expected NoSuchElementException INCORRECT")
    }
    val a = Factory(1)
    val b = Factory(2)
    a.tail = b
    check("a.tail = b => a.tail == b", a.tail, b)
    check("a.length == 2", a.length, 2)
    a.tail = Factory.empty[Int]
    check("a.tail.isEmpty", a.tail.isEmpty, true)
    check("a.length == 1", a.length, 1)
  }
  // testClear
  // testGet
  // testUpdate
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
    println("Done testing: " + testName)
  }
}

import scala.collection.mutable.LinkedList

object LinkedListTest extends TestSuite[LinkedList] {
  val Factory = LinkedList
  val testName = "LinkedList"
}

object Test {
  def main(args: Array[String]) {
    LinkedListTest.run()
  }
}
