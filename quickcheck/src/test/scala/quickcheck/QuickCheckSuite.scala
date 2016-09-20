package quickcheck

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException

object QuickCheckBinomialHeap extends QuickCheckHeap with BinomialHeap

@RunWith(classOf[JUnitRunner])
class QuickCheckSuite extends FunSuite with Checkers {
  def checkBogus(p: Prop) {
    var ok = false
    try {
      check(p)
    } catch {
      case e: TestFailedException =>
        ok = true
    }
    assert(ok, "A bogus heap should NOT satisfy all properties. Try to find the bug!")
  }
  /*
  test("Binomial heap satisfies toList.") {
   val H = new QuickCheckHeap with quickcheck.test.BinomialHeap
   val h1 = H.insert(4, H.empty)
   val h2 = H.insert(1, h1)
   val h3 = H.insert(3, h2)
   val h4 = H.insert(2, h3)
   assert(h4.toList.map(v => v.x) == List(1,2,3,4))
  }
		*/
  test("Binomial heap satisfies properties.") {
    check(new QuickCheckHeap with quickcheck.test.BinomialHeap)
  }

  test("Bogus (1) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus1BinomialHeap)
  }

  test("Bogus (2) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus2BinomialHeap)
  }

  test("Bogus (3) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus3BinomialHeap)
  }

  test("Bogus (4) binomial heap does not satisfy properties.") {
    println("======================================================")
    println("BOGUS 4")
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus4BinomialHeap)
    println("END BOGUS 4")
    println("======================================================")

  }

  test("Bogus (5) binomial heap does not satisfy properties.") {
    checkBogus(new QuickCheckHeap with quickcheck.test.Bogus5BinomialHeap)
  }

  
}
