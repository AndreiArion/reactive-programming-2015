package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert min and then find Min should return the min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert two elements in a empty heap and then find min should find the min of two ") = forAll { (v1: A, v2: A) =>
    val m = min(v1, v2)
    findMin(insert(v2, insert(v1, empty))) == m
  }

  property(" insert to empty heap than delete should give empty ") = forAll { a: A =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("findMin on a meld should be the min of the two findMin") = forAll { (h1:H,h2:H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    findMin(meld(h1,h2)) == min(m1,m2)
  }

  property("recursively calling find/deleteMin generates an ordered sequence") = forAll { (h:H) =>
    val l = elemList(h)
    isAscending(l) == true
  }

  property("melding two heaps and getting elems = getting the elemms on the original heap") = forAll{ (h1:H,h2:H) =>
    val l1 = elemList(h1)
    val l2 = elemList(h2)
    val l :List[A] = l1++l2
    val meldedHeap = meld(h1,h2)
    val meldedElem = elemList(meldedHeap)
    l.sorted(Ordering[A]).reverse == meldedElem
  }



  def isAscending(l:List[A])={
    val sortedList = l.sorted.reverse
    //l zip sortedList map (x => x._1==x._2)
    l == sortedList
  }

  def elemList(h: H):List[A] = {
    def elemListRect(h:H,curList: List[A]):List[A]={
      if(isEmpty(h))return curList
      else{
        val el = findMin(h);
        elemListRect(deleteMin(h),el::curList)
      }
    }
    elemListRect(h,Nil)
  }

  def min(x: A, y: A) = if (x < y) x else y

  lazy val genHeap: Gen[H] = for {
    el <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(el, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
