package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  
  
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = 
    namedExpressions.map(x => (x._1, Signal(eval(x._2(),namedExpressions)))).toMap
//
  

//  def isCyclic(expr: Expr, references: Map[String, Signal[Expr]]): Boolean {
//    if(
//  }
  
   
  
  def eval(expr: Expr, references: Map[String, Signal[Expr]], refs :Set[String] = Set[String]() ): Double = {
      
     //var refs = Set[String]()

     expr match {
       case Literal(e) => e
       case Plus(a,b) => eval(a,references,refs) + eval(b,references,refs)
       case Minus(a,b) => eval(a,references,refs) - eval(b,references,refs)
       case Times(a,b)  => eval(a,references,refs) * eval(b,references,refs)
       case Divide(a,b)  => eval(a,references,refs) / eval(b,references,refs)
       case Ref(s) => {
         if(refs.contains(s)) Double.NaN
         else eval(getReferenceExpr(s,references),references, refs+s)
       }
    }
    
    
  }

  
  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] { Literal(Double.NaN) } { exprSignal =>
      exprSignal()
    }
  }
}
