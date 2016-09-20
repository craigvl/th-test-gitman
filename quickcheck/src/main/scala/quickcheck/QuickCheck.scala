package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // For a map
  /*
  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)
	*/
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  
   def fromList(l: List[A], h: H): H = {
    if (l.isEmpty) h
    else {
      fromList(l.tail, insert(l.head, h))
    }
  }
  
  def removeAll(h: H, removed: List[Int]): List[Int] = {
    if (isEmpty(h)) removed
    else {
      val min = findMin(h)
      removeAll(deleteMin(h), removed ::: List(min))
    }
  }
  
  property("delFindMin") = forAll { (h: H) =>
    val all = removeAll(h, List())
    all == all.sorted
  }
  
  property("oneDelIns") = forAll { (h: H) =>
    val all = removeAll(h, List())
    
    val h1 = deleteMin(h)
    val all1 = removeAll(h1, List())
    
    all.tail == all1 &&  all1 == all1.sorted && all == all.sorted
  }
  
  property("delAdd") = forAll { (h: H) =>
    val all = removeAll(h, List())
    val min = all.sorted.head
    
    val h1 = deleteMin(h)
    val all1 = removeAll(h1, List()).sorted
    
    val h2 = insert(min, h1)
    
    val resAll = removeAll(h2, List())
    
    all == resAll &&  resAll == resAll.sorted && all == all.sorted
  }
  
  property("allDelAdd") = forAll { (h: H) =>

    val all = removeAll(h, List())
    println("all:"+all)
    val min = all.head
    println("min:"+min)
    
    val h1 = deleteMin(h)
    val all1 = removeAll(h1, List())
    println("all1:"+all1)

    val h2 = insert(min, h1)
    
    val resAll = removeAll(h2, List())
    println("all2:"+resAll)

    all == resAll && resAll == resAll.sorted && all == all.sorted
  }
  
  property("allDelIns") = forAll { (h: H) =>
    val all: List[A] = removeAll(h, List())

    val newH = fromList(all, empty);
    val newAll: List[A] = removeAll(newH, List())

    all == all.sorted && newAll == all && newAll == newAll.sorted
      
  }
  
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("min2") = forAll { (a: Int, b:Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    if(a < b) findMin(h2) == a
    else findMin(h2) == b
  }
  
  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    empty == deleteMin(h)    
  }
  
  property("del2") = forAll { (a: Int, b:Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    

    val h3 = deleteMin(h2)   
    // one elem should remains, the greater of the two
    if(a < b) h3 == insert(b, empty) 
    else h3 == insert(a, empty)
  }
  
  property("findMin and deleteMin from a random heap should yield a sorted sequence") = forAll { h: H =>
    def rec(h: H): List[Int] = h match {
      case h if h == empty => Nil
      case h => findMin(h) :: rec(deleteMin(h))
    }
    val l = rec(h)
    (l, l.tail).zipped.forall(_ <= _)
  }


  property("recMin") = forAll { h: H =>
    def rem(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: rem(deleteMin(ts), as)
    }
    val xs = rem(h, Nil)
    xs == xs.sorted
  }
  
  property("sorted2") = forAll { h : H =>
    if(isEmpty(h)) true
    else {
      val m1 = findMin(h)
      val h1 = deleteMin(h)  
      
      if(isEmpty(h1)) true
      else {
        val m2 = findMin(h1)
        m1 <= m2
      }

    }
    
    

  }
  
  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  
  property("delAll") = forAll { initH : H =>
    // toOrderedList
    def iter(h : H, l : List[Int]) : Boolean = {
      if(isEmpty(h)) true
      else {
        val m = findMin(h)
        val rH = deleteMin(h)
        
        val rL = l ::: List(m)
        if(rL.sorted == rL) iter(rH, l ::: List(m))
        else false
        
      }
    }
    
    iter(initH, List[Int]())
    
      
  }
  
  def toList(h: H): List[Int] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }
  /*
  def toListOnlyDeleteMin(h: H): List[Int] = {
    val rem = deleteMin(h)
    if (isEmpty(rem)){
      val deleted = findMin(h)
      deleted :: toListOnlyDeleteMin(h)
    }
    
    if (isEmpty(h)) Nil
    else toList(deleteMin(h)) :: 
  }*/
  
  property("sorted9") = Prop.forAll { (h: H) =>
		def isSorted(last: Int, heap: H): Boolean = {
			isEmpty(heap) || {
				val min = findMin(heap)
				last <= min && isSorted(min, deleteMin(heap))
			}
		}

		isEmpty(h) || isSorted(findMin(h), deleteMin(h))
	}

  def isSorted(xs: List[Int]): Boolean = xs match {
    case Nil => true
    case x :: xss => if (xss.isEmpty) true else (x <= xss.head) && isSorted(xss)
  }
  
  property("sorted") = forAll { h: H =>
    val l = toList(h)
    isSorted(l)
  }
  
  /*
  property("deleteMin") = forAll { h: H =>
    if(isEmpty(h)) true
    
    var heap = h
    var outcome = true
    
    var pathAfter = List[Int]()
    var pathBefore = List[Int]()

    while(!isEmpty(heap) && outcome){
      val minBefore = findMin(heap)
      pathBefore =  pathBefore ::: List(minBefore)
      val rest = deleteMin(heap)

      val minAfter = if (!isEmpty(rest)) findMin(rest) else Int.MaxValue
      pathAfter = if (!isEmpty(rest)) pathAfter ::: List(minAfter) else pathAfter
      heap = rest
      
      if(minBefore <= minAfter) 
        outcome = true 
      else 
        outcome = false
    }
    println("============================")
    println(pathBefore mkString ",")
    println(pathAfter mkString ",")

    var newH = empty
    for(v <- pathAfter)
      newH = insert(v, newH)
    
    
    outcome && pathBefore == pathBefore.sorted && pathAfter == pathAfter.sorted && newH == h
  }
*/

  /*
  property("checkAllSteps") = forAll { h : H =>
    
    var currH = h;
    var isCorrect = true      

    while(!isEmpty(currH) && isCorrect){
      
      // sanity checks
      val currL = toList(currH);      
      
      if(currL.sorted != currL) isCorrect = false
      
      val foundMin = findMin(currH);      
      if(!currL.contains(foundMin)) isCorrect = false

      // delete 
      val newH = deleteMin(currH);      
      val newL = toList(newH);      
      
      if(newL != newL.sorted) isCorrect = false
      
      if(currL.tail != newL) isCorrect = false
      
      
      currH =  newH
    }
    
    isCorrect
      
  }*/
  
  property("sortedAllPure") = forAll { initH : H =>
    // toOrderedList
    def iter(h : H, l : List[Int]) :  List[Int] = {
      val r = deleteMin(h)
      if(isEmpty(r)) findMin(h) :: l
      else toList(r)     
    }
    
    if(isEmpty(initH)) true else {
      val r = iter(initH, List[Int]())
      r.sorted == r
    }
  }

  property("sortedAll") = forAll { initH : H =>
    // toOrderedList
    def iter(h : H, l : List[Int]) :  List[Int] = {
      if(isEmpty(h)) l
      else {
        val m = findMin(h)
        val rH = deleteMin(h)
        iter(rH, l ::: List(m))
      }
    }
    
    val r = iter(initH, List[Int]())

    r.sorted == r
      
  }
  
    property("meldEmpty") = forAll { (h : H) =>
      
      val h1 = meld(empty,h)
      val h2 = meld(h,empty)

      h == h1 && h == h2 && h1 == h2

  }
  
  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld2") = forAll { (h1 : H,  h2 : H) =>
    
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    
    val h = meld(h1,h2)
    val m = findMin(h)

    m == m1 || m == m2

  }
  
  property("meldAss") = forAll { (h1 : H,  h2 : H, h3 :H) =>
    
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m3 = findMin(h3)

    val hA = meld(meld(h1,h2),h3)
    val hB = meld(h1,meld(h2,h3))
    val hC = meld(h3,meld(h2,h1))
    
    val mA = findMin(hA)
    val mB = findMin(hB)
    val mC = findMin(hC)
    
    (mA == mB && mA == mC) && (mA == m1 || mA == m2 || mA == m3)
  }
  
}
