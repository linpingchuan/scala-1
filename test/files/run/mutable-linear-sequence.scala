

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
    println("\t\t" + label + " == " + result + " " + correctness)
  }
  def checkNSE(label: String, op: => Any) {
    try {
      op
      println("\t\t" + label + " should have thrown a NoSuchElementException INCORRECT")
    } catch {
      case e: NoSuchElementException => println("\t\t" + label + " threw a NoSuchElementException CORRECT")
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
    check("c ++= " + baseList + " => c" + baseList, c, doubledList)
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
