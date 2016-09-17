package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  val s = union(union(singletonSet(1),singletonSet(2)),singletonSet(3))
  printSet(s)

}
