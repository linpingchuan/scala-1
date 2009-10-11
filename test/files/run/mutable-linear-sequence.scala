

import scala.collection.LinearSequence
import scala.collection.mutable.Sequence
import scala.collection.generic.{ SequenceFactory, GenericTraversableTemplate }

abstract class TestSuite[CC[X] <: Sequence[X] with GenericTraversableTemplate[X, CC] with LinearSequence[X]] {
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
  }
  def testSingular() {
    println("\tTesting singular: " + testName)
    val a = Factory(1)
    check("isEmpty =", a.isEmpty, false)
    check("length =", a.length, 1)
    check("head =", a.head, 1)
    check("tail.isEmpty =", a.tail.isEmpty, true) 
  }
  def run() {
    println("Testing: " + testName)
    testEmpty()
    testSingular()
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
