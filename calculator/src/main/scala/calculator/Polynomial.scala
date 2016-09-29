package calculator

object Polynomial {
  //Δ = b² - 4ac
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
     Signal(b()*b() - 4*a()*c())
  }

  
  def f(a: Signal[Double], b: Signal[Double])(x: Signal[Double]) = (-b()+x()) / (2*a())
  
  // (-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = 
   c() match { 
      case 0 => Signal(Set(f(a,b)(Signal(0))))
      case x if x < 0 => Signal(Set())
      case _ => Signal(Set(f(a,b)(Signal(Math.sqrt(delta())) ),f(a,b)(Signal(-1*Math.sqrt(delta())) )))    
  }
}

