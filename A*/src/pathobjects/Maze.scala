package pathobjects

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 31/07/13
 * Time: 11:58
 * To change this template use File | Settings | File Templates.
 */

object Maze {
	var w = 10
	var h = 10
	var maze = Array.ofDim[Nodo](w, h)
	def setSize(w: Int, h:Int) {
		maze = Array.ofDim[Nodo](w, h)
		this.w = w
		this.h = h
		calcMaze
	}
	def calcMaze {
		for (i <- 0 to w - 1) for (e <- 0 to h - 1) maze(i)(e) = new Nodo(i, e)
		for (i <- 0 to w - 1) for (e <- 0 to h - 1) maze(i)(e) setNeightbours
	}
	def get(x: Int, y: Int): Nodo = {
		if (x >= 0 && y >= 0 && x < w && y < h) maze(x)(y) else new Nodo(x, y)
	}
}
