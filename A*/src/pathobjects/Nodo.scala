package pathobjects

import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 31/07/13
 * Time: 11:55
 * To change this template use File | Settings | File Templates.
 */
class Nodo(var x: Int, var y: Int) {
	var father: Nodo = this
	var cost: Double = 0
	var h: Double = 0
	var g: Double = 0
	val neightbours = ListBuffer.empty[Nodo]
	def setH(h:Double){
		this.h=h
		cost = g + h
	}
	def setG(g:Double){
		this.g=g
		cost = g + h
	}
	def setValue(g:Double, h:Double){
		this.h=h
		this.g=g
		cost = g+h
	}
	var walkable = true
	def isValid = x >= 0 && y >= 0 && x < Maze.w && y < Maze.h && walkable
	def isValidNode(node :Nodo):Boolean = {
		return (node.x >= 0 && node.y >= 0 && node.x <= Maze.w - 1 && node.y <= Maze.h - 1 && node.walkable)
	}
	def isTheSame(nodo: Nodo) = nodo.x == x && nodo.y == y
	def getNeightbours = neightbours
	def setNeightbours = {
		neightbours.clear()
		if(x>0 && Maze.get(x - 1, y).walkable) neightbours += Maze.get(x - 1, y)
		if(x<Maze.w-1 && Maze.get(x + 1, y).walkable) neightbours +=Maze.get(x + 1, y)
		if(y<Maze.h-1 && Maze.get(x, y+1).walkable) neightbours += Maze.get(x, y + 1)
		if(y>0 && Maze.get(x, y-1).walkable) neightbours += Maze.get(x, y - 1)
	}
	//def getNeightbours = Maze.get(x - 1, y) :: Maze.get(x + 1, y) :: Maze.get(x, y + 1) :: Maze.get(x, y - 1) :: Maze.get(x - 1, y + 1) :: Maze.get(x - 1, y -1) :: Maze.get(x + 1, y + 1) :: Maze.get(x + 1, y - 1) :: Nil
}
