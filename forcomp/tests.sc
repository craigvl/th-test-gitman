

object tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  type Word = String
  type Occurrences = List[(Char, Int)]
  
  def wordOccurrences(w: Word): Occurrences = w.groupBy(g => g.toLower).map({ case (k,v) => (k,v.length()) }).toList
                                                  //> wordOccurrences: (w: tests.Word)tests.Occurrences
  
  def directSubsets(l : Occurrences = List(), h : Occurrences = List(), subs : List[Occurrences] = List()) : List[Occurrences] =
  	 l match {
  	 	 case Nil => List()
  	 	 case x :: xs => (h ::: (if(x._2 > 1) List((x._1, x._2-1)) else List()) ::: xs) :: directSubsets(xs, h ::: List(x) )
  }                                               //> directSubsets: (l: tests.Occurrences, h: tests.Occurrences, subs: List[tests
                                                  //| .Occurrences])List[tests.Occurrences]
  	 
  
  //directSubsets(List(('a', 2), ('b', 1), ('c', 2)))


/*
def permutations(s: Occurrences): List[Occurrences] = {
    def merge(ins: Occurrences, c: (Char, Int)): Seq[Occurrences] =
      for (i <- 0 to ins.length) yield
        ins.slice(0, i) ::: List(c) ::: ins.slice(i, ins.length)

    if (s.length == 1)
      List(s)
    else
      permutations(s.slice(0, s.length - 1)).flatMap { p =>
        merge(p, p.last)
      }
  }
*/

	def permutations[T](ls: List[T]): List[List[T]] = ls match {
	  case List() => List(List())
	  case _ => for(e <- ls; r <- permutations(ls filterNot(_==e))) yield e::r
	}                                         //> permutations: [T](ls: List[T])List[List[T]]

  //permutations(List(('a', 2), ('b', 1), ('c', 2)))
  
  
  def subsets(l : Occurrences) : List[Occurrences] =
  	if(l.length == 0) l :: List[Occurrences]()
  	else l :: directSubsets(l).flatMap(s => subsets(s)).distinct
                                                  //> subsets: (l: tests.Occurrences)List[tests.Occurrences]
  


 	subsets(List(('a', 2), ('b', 2))) mkString("\n")
                                                  //> res0: String = List((a,2), (b,2))
                                                  //| List((a,1), (b,2))
                                                  //| List((b,2))
                                                  //| List((b,1))
                                                  //| List()
                                                  //| List((a,1), (b,1))
                                                  //| List((a,1))
                                                  //| List((a,2), (b,1))
                                                  //| List((a,2))

 	
 	


 /*
  def perms(set : Occurrences, acc : List[Occurrences] =  List()) : List[Occurrences] =
  	set :: directSubsets(set).flatMap(s => s match {
  	  case x :: xs => set :: (if(x._2 > 1) perms((x._1, x._2-1) :: xs) else perms(xs)) ::: acc
  	//	case x :: xs => set :: directSubsets(xs).flatMap(s => perms(s,acc) ::: acc)
  	  case x :: Nil => set :: (if(x._2 > 1) perms(List((x._1, x._2-1))) else perms(List())) ::: acc ///set :: (if(x._2 > 1) perms(List((x._1, x._2-1))) else perms(List())) ::: subsets
  		case Nil => set :: acc
  	}  )
                                                    		*/
   /*
  *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
  		
  		*/
  		//perms(List(('a', 2), ('b', 2))) mkString("\n")
  		
  	      
  		//val lst = List(('a', 2), ('b', 2))
  	//	directSubsets(lst).flatMap(l => perms(l)) mkString("\n")
  		
  		
  		
  		
  		
  //perms(List(('a', 2), ('b', 2))).flatMap(l => directSubsets(l))
  		
	//perms(List(('a', 2), ('b', 2),('c',1))).flatMap(l => directSubsets(l))
	
	
	
}