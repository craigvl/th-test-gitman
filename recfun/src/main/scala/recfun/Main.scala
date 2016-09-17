package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }
  
  /**
   * Exercise 1
   */
  def triangle(col : Int, r : Int, row: List[Int] = List(1)) : Int = {
    	val currentLevel = row.length;
      if(currentLevel == r){
    		 return row.take(col).last;
    	}
    	else{
    	  val nextRow = ((0::row):::List(0)).sliding(2).map(v => v.head + v.last).toList;
    		return triangle(col, r, nextRow);
    	}
    }  
  
  def pascal(c: Int, r: Int): Int = {
    return triangle(c+1, r+1);
  }
     


  /**
   * Exercise 2
   */
def balance(chars: List[Char], countLeft : Int = 0): Boolean = {
                
				if(chars.isEmpty) 
				  return countLeft == 0;
				else if(chars.head == ')' && countLeft == 0) 
				  return false
				else if (chars.head == ')' && countLeft > 0) 
				  return balance(chars.tail, countLeft - 1) 				
				else if(chars.head == '(') 
				  return balance(chars.tail, countLeft + 1) 
				else 
				  return balance(chars.tail, countLeft) 

   		}
   
   		 var p = 0 ;
       //var sets = Set[List[Int]]();
       def perms (n : Int, coins : List[Int]){// : Int = {    
        val permCoins = coins.filter(c => (c <= n)).toList;   
        //var sumPerms = p;        
        
        for(coin <- permCoins){
          
          if(n-coin == 0) {            
            p = p + 1;
          }      
          else if(n-coin > 0){
            perms(n - coin, permCoins.filter(c => (c <= coin)).toList);
          }
          /*else {
            sumPerms 0;
          }*/
          
        }
        //return sumPerms;
      };
  
  /**
   * Exercise 3
   */
   def countChange(money: Int, coins: List[Int]): Int = {
     p = 0 ;
     val sortedCoins = coins.sorted;
     perms(money, coins);
     return p;
         
   }
  }


