package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val twa = new Tweet("a", "a body", 20)
    val set2 = set1.incl(twa)
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    set3.filter(tw => tw.user == "a")
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

   test("filter: a on set5 (all true)") {
    new TestSets {
      println(" filter: a on set3");
      assert(size(set5.filter(tw => true)) === 4)
    }
  }
  
  test("filter: a on set5 2") {
    new TestSets {
      println(" filter: a on set5");
      assert(size(set5.filter(tw => true)) === 4)
    }
  }
  
  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => true)) === 0)
    }
  }
  

  
 test("remove: a on set3") {
    new TestSets {
     // val s = size(set3.remove(twa));
      assert(size(set3.remove(twa)) === 1)
    }
  }
 
  test("remove: a on set5") {
    new TestSets {
      assert(size(set5.remove(twa)) === 3)
    }
  }
  
  

  test("filter: a on set5") {
    new TestSets {
      println(" filter: a on set5");
      set5 foreach println
      val s  = set5.filter(tw => tw.user == "a");
      println(" filter: a on set5");
      s foreach println
      assert(size(s) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }
  
    test("most: retweets = 9") {
    new TestSets {
      assert(set5.mostRetweeted.retweets === 20)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

  }
