object tests {

  println("heello")

  val t = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
	case class Pos(x: Int, y: Int)
	def terrainFunction(levelVector: Vector[Vector[Char]]): (Pos) => Boolean = (pos) => (pos.x >= 0 && pos.x < levelVector.length) && (pos.y >= 0 && pos.y < levelVector(pos.x).length) && (levelVector(pos.x))(pos.y) != '-'
		      
		     
		      
		    


	val x = terrainFunction(t)
  val r = x(Pos(-1,1))
}