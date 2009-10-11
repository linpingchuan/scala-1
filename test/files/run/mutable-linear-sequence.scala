

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
    check("list == List(1) =", list == singular, true)
    list += 2
    check("list == " + double, list == double, true)
    list += 3
    check("list == " + triple, list == triple, true)
    list += 4
    check("list == " + baseList, list == baseList, true)
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
    check("b ++= Nil => b == " + baseList, b == baseList, true)
    val c = Factory(1, 2, 3, 4)
    c ++= baseList
    check("c ++= " + baseList + " => c == " + baseList, c == doubledList, true)
    val d = Factory(1)
    d ++= List(2)
    check("d ++= List(2), d == " + double, d == double, true)
  }
  def testAppend() {
    println("\tTesting appending a list of same type: " + testName)
    val a = Factory(1, 2, 3, 4)
    val b = Factory(1, 2, 3, 4)
    a.append(b)
    check("a.append(b) => a == " + doubledList, a == doubledList, true)
    val e = Factory.empty[Int]
    val c = Factory(1, 2, 3, 4)
    e.append(c)
    check("e.append(c) => e == " + baseList, e == baseList, true)
    val e2 = Factory.empty[Int]
    val d = Factory(1, 2, 3, 4)
    d.append(e2)
    check("d.append(e2) => d == " + baseList, d == baseList, true)
  }
  // testInsert
  // testAssignHead
  // testAssignTail
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
