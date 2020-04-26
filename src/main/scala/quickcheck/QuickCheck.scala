package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      h <- arbitrary[A]
      g <- oneOf(const(empty), genHeap)
    } yield insert(h, g)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val minValue = if(a <  b) a else b
    findMin(h) == minValue
  }

  property("insert+delete1") = forAll { a: Int =>
    val h = insert(a, empty)
    val d = deleteMin(h)
    isEmpty(d)
  }

  property("sort1") = forAll { h:H =>
    def aux(heap: H, min: A) : Boolean = {
      if(isEmpty(heap)) {
        true
      } else {
        val newMin = findMin(heap)
        if(newMin >= min) aux(deleteMin(heap), newMin)
        else false
      }
    }

    if(isEmpty(h)) true
    else aux(deleteMin(h), findMin(h))
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    val min1 = if(isEmpty(h1)) None else Some(findMin(h1))
    val min2 = if(isEmpty(h2)) None else Some(findMin(h2))
    val minOfBoth = (min1, min2) match {
      case (Some(a), Some(b)) => if(a < b) a else b
      case (Some(a), None) => a
      case (None, Some(b)) => b
      case _ => 0
    }
    val minM = if(isEmpty(m)) 0 else findMin(m)

    minM == minOfBoth
  }

  property("meld2") = forAll { (h1: H, h2: H) =>

    def aux(heap: H, min: A) : Boolean = {
      if(isEmpty(heap)) {
        true
      } else {
        val newMin = findMin(heap)
        if(newMin >= min) aux(deleteMin(heap), newMin)
        else false
      }
    }
    val m = meld(h1, h2)
    if(isEmpty(m)) true
    else aux(deleteMin(m), findMin(m))
  }

  property("meld3") = forAll { a: A =>
    val h1 = insert(a, empty)
    val h2 = empty
    val m = meld(h1, h2)

    findMin(m) == a
  }

  property("meld4") = forAll { a: A =>
    val h1 = insert(a, empty)
    val h2 = insert(a, empty)
    val m = meld(h1, h2)

    findMin(m) == a
  }

  property("meld5") = forAll { a: A =>
    val h1 = insert(a, empty)
    val h2 = insert(a, empty)
    val m = meld(h1, h2)
    val r = deleteMin(m)

    findMin(r) == a
  }

  property("meld6") = forAll { h: H =>
    val m = meld(h, empty)
    m == h
  }

  property("findMin1") = forAll { (a: A, b: A, c: A) =>

    def min = (a: Int, b:Int) => if (a < b) (a, b) else (b, a)

    val (m1, m2, m3) = {
      val (t1, t2) = min(a, b)
      val (m1, t3) = min(t1, c)
      val (m2, m3) = min(t2, t3)
      (m1, m2, m3)
    }

    val h1 = insert(c, insert(b, insert(a, empty)))
    findMin(h1) == m1

    val h2 = deleteMin(h1)
    findMin(h2) == m2

    val h3 = deleteMin(h2)
    findMin(h3) == m3

  }

}
