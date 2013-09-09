package pathobjects

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 09/08/13
 * Time: 19:25
 * To change this template use File | Settings | File Templates.
 */
trait dCalc {
	def calc(nodoA: Nodo, nodoB: Nodo):Float
}

object dCalc{
	var multiplier = 1f
}

object ceroCost extends dCalc{
	def calc(nodoA: Nodo, nodoB: Nodo) = 0
}

object manhattanDistance extends dCalc{
	def calc(nodoA: Nodo, nodoB: Nodo) = ((nodoA.x - nodoB.x).abs + (nodoA.y - nodoB.y).abs) *  dCalc.multiplier
}

object euclideanDistance extends dCalc{
	def calc(nodoA: Nodo, nodoB: Nodo)= math.sqrt( math.pow(nodoA.x - nodoB.x, 2) + math.pow(nodoA.y - nodoB.y, 2) ) * dCalc.multiplier toFloat
}

object rareDistance extends dCalc{
	def calc(nodoA: Nodo, nodoB: Nodo)=((nodoA.x - nodoB.x).abs - (nodoA.y - nodoB.y)).abs * dCalc.multiplier
}

object chebyshevCost extends dCalc{
	def calc(nodoA: Nodo, nodoB: Nodo):Float={
		val a = (nodoA.x - nodoB.x).abs
		val b = (nodoA.y - nodoB.y).abs
		if(a > b) return a.toFloat * dCalc.multiplier
		return b.toFloat * dCalc.multiplier
	}
}