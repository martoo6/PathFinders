package runnables

import processing.core._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.Predef._
import controlP5._
import pathobjects._
import scala.util.Random

class PathFinder extends PApplet{

	val controlSize=300
	val screenW = 1000
	val screenH = 700
	var drawText=false
	var stepX :Float=(screenW-controlSize)/Maze.w
	var stepY :Float=screenH/Maze.h
	val textsize = ((stepX+stepY)*0.5*0.16).toFloat
	var cp5:ControlP5 = _
	var showSteps = false
	var sleepTime = 0
	var currentSelection = 0
	var solve = false
	var solved = false
	var lastTime = 0
	var showGrid= false
	var currentCalculator:dCalc = euclideanDistance

	val ord = new Ordering[Nodo] {
		def compare(n1: Nodo, n2: Nodo): Int = (n1.cost-n2.cost).toInt
	}


	var notWalkable = mutable.HashMap.empty[Int, Nodo]
	var open= ListBuffer.empty[Nodo]
	//var open= mutable.SortedSet.empty[Nodo](ord)
	var solution= ListBuffer.empty[Nodo]
	val closed = mutable.HashMap.empty[Int,Nodo]
	var orig = new Nodo(-1,-1)
	var dest= new Nodo(-1,-1)

	override def setup {
		size(screenW, screenH)
		createGUI


		//----VARIABLES INIT---
		solve = false
		showSteps = false
		drawText=false

		Clear
	}


	def createGUI {
		cp5 = new ControlP5(this)
		val newMazeListener = new ControlListener {
			def controlEvent(p1: ControlEvent) = Clear
		}

		quickPos.initQuickPos(screenW - controlSize + 20, 20)

    quickPos.nextQuickItem(
			cp5.addButton("NewMaze")
				.setValue(0)
				.setCaptionLabel("Nuevo")
				.addListener(newMazeListener)
				.setSize(100,40)
		)

    quickPos.linebreakQuickItem(
			cp5.addButton("Solve")
				.setCaptionLabel("Resolver")
				.setSize(100,40)
		)

    quickPos.linebreakQuickItem(0, 80,
			cp5.addRadioButton("currentSelection")
				.addItem("Origen", 0)
				.addItem("Destino", 1)
				.addItem("Libre", 2)
				.addItem("Ocupado", 3)
				.setItemsPerRow(2)
				.setSpacingColumn(50)
				.setSize(60, 40)
				.activate(0)
		)

    quickPos.linebreakQuickItem(
			cp5.addButton("RandomValues")
				.setCaptionLabel("Generar al azar")
				.setSize(100, 40)
				.addListener(new ControlListener {
				def controlEvent(p1: ControlEvent) {
					notWalkable.clear()
					clearLists
					Maze.setSize(Maze.w, Maze.h)
					for (i <- 0 to Maze.w - 1) for (e <- 0 to Maze.h - 1) {
						if (Random.nextInt(10) < 1) {
							val nodo = Maze.get(i, e)
							nodo.walkable = false
							notWalkable += ((nodo.hashCode(), nodo))
						}
					}
					for (i <- 0 to Maze.w - 1) for (e <- 0 to Maze.h - 1) Maze.get(i, e).setNeightbours
				}
			})
		)


    quickPos.linebreakQuickItem(
			cp5.addSlider("HeuristicMultiplier")
				.setMax(20)
				.setSize(150, 40)
				.setCaptionLabel("Multiplicador de Heuristica")
			  .setNumberOfTickMarks(21)
				.setValue(1)

		)

    quickPos.nextQuickItem(
			cp5.addToggle("ShowSteps")
				.setColorForeground(color(120))
				.setColorActive(color(255))
				.setCaptionLabel("Pasos")
				.setSize(70,40)
		)

    quickPos.nextQuickItem(
			cp5.addToggle("ShowValues")
				.setColorForeground(color(120))
				.setColorActive(color(255))
				.setCaptionLabel("Valores")
				.setSize(70,40)
		)

    quickPos.linebreakQuickItem(0,10,
			cp5.addToggle("ShowGrid")
				.setColorForeground(color(120))
				.setColorActive(color(255))
				.setCaptionLabel("Mostrar Grilla")
				.setSize(70,40)
		)

    quickPos.linebreakQuickItem(0, 30,
			cp5.addSlider("sleepTime")
				.setMax(3000)
				.setSize(150, 30)
		)

		val l = cp5.addDropdownList("DistanceCalculator")
			.setCaptionLabel("Calculadora de Distancia")
			.setItemHeight(25)
			.setSize(150,100)
			.setBarHeight(25)

		l.addItem("Manhattan", 0)
		l.addItem("Euclidean", 1)
		l.addItem("Rare", 2)
		l.addItem("Chebyshev", 3)
		l.addItem("Cero", 4)

		l.addListener(new ControlListener {
			def controlEvent(p1: ControlEvent) {
				l.getItem(p1.getValue.toInt).getText match {
					case "Manhattan" => currentCalculator = manhattanDistance
					case "Euclidean" => currentCalculator = euclideanDistance
					case "Rare" => currentCalculator = rareDistance
					case "Chebyshev" => currentCalculator = chebyshevCost
					case "Cero" => currentCalculator = ceroCost
				}
			}
		})

    quickPos.linebreakQuickItem(0, 50, l)

    quickPos.nextQuickItem(
			cp5.addSlider2D("MazeSize")
				.setSize(170, 170)
				.setArrayValue(Array(0f, 0f))
				.setMaxX(300)
				.setMaxY(300)
				.setMinX(3)
				.setMinY(3)
				.setCaptionLabel("N de celdas")
				.addListener(newMazeListener)
		)
	}

	override def draw {
		frame.setTitle(frameRate.toString)
		background(0)

		if(solved) {
			drawMaze
			fill(255)
			textSize(12)
			text("Largo de recorrido: "+ solution.size, screenW-controlSize+20, screenH-20)
		}
		if(solve){
			drawMaze
			if (showSteps){
				if(millis()-lastTime>sleepTime){
					solver
					lastTime = millis()
				}
			} else {
				while(open.size > 0 && !dest.isTheSame(open.head)) solver
			}
			if (open.size >0 && dest.isTheSame(open.head)){
				while(solution(solution.size-1)!=orig) solution+=solution(solution.size-1).father
				solve = false
				solved = true
			}
		}
		if(mousePressed){
			if(mouseX>0 && mouseX<screenW-controlSize && mouseY>0 && mouseY< screenH){
				clearLists
				val nodo = Maze.get(math.floor(mouseX/stepX).toInt, math.floor(mouseY/stepY).toInt)
				currentSelection match{
					case 0 => orig = nodo
					case 1 => dest = nodo
					case 2 => nodo.walkable = true; notWalkable.remove(nodo.hashCode())
					case 3 => nodo.walkable = false; for(node<-nodo.getNeightbours) node.setNeightbours; if(!notWalkable.contains(nodo.hashCode())) notWalkable+=((nodo.hashCode(),nodo))
					case _ => ;
				}
			}
		}
		drawGrill
		drawEmptyMaze
	}


	def clearLists {
		solution.clear()
		closed.clear()
		open.clear()
	}

	def solver {
		val current = open.head
		closed.put(current.hashCode(), current)
		open-=current
		//var s1 = new Semaphore(1)
		for (node <- current.neightbours){
			if(!closed.contains(node.hashCode())){
				val currentCost = current.g + 1 + currentCalculator.calc(node,dest)
				if(!open.contains(node)){
					node.father = current
					node.setValue(current.g + 1, currentCalculator.calc(node,dest))
					open+=node
				}else{
					if (node.cost > currentCost) {
						open-=node
						node.father = current
						node.setValue(current.g + 1, currentCalculator.calc(node,dest))
						open+=node
					}
				}
				//s1.acquire()
				//if(!open.contains(node)) open+=node //ESTO ESTA CON LISATAS COMUNES !
				//s1.release()
			}
		}
		open = open.sortBy(_.cost)
	}


	def drawGrill(){
		if(showGrid){
			stroke(255)
			for (i <- 0 to Maze.w) line(i*stepX,0,i*stepX,screenH)
			for (e <- 0 to Maze.h) line(0,e*stepY,screenW-controlSize,e*stepY)
		}
	}

	def drawEmptyMaze {
		noStroke()
		fill(60)
		for(nodo <-notWalkable) rect(nodo._2.x*stepX, nodo._2.y*stepY, stepX, stepY)
		fill(255,255,0)
		if(orig.x != -1) rect(orig.x*stepX, orig.y*stepY, stepX, stepY)
		fill(200,0,0)
		if(dest.x != -1) rect(dest.x*stepX, dest.y*stepY, stepX, stepY)
	}

	def drawMaze= {
		noStroke()
		fill(60)
		for(nodo <-notWalkable) rect(nodo._2.x*stepX, nodo._2.y*stepY, stepX, stepY)
		fill(0,0,255)
		for(nodo <-open) rect(nodo.x*stepX, nodo.y*stepY, stepX, stepY)
		fill(255,0,0)
		for(value <-closed) rect(value._2.x*stepX, value._2.y*stepY, stepX, stepY)
		fill(0,255,0)
		for(nodo <-solution) rect(nodo.x*stepX, nodo.y*stepY, stepX, stepY)
		if(drawText)	for (i <- 0 to Maze.w-1; e <- 0 to Maze.h-1)  drawValues(Maze.get(i, e))
	}

	def drawValues(nodo: Nodo) {
		if(drawText){
			fill(255)
			text(nodo.cost.toString.take(4), nodo.x * stepX + stepX*0.1f, nodo.y * stepY + textsize)
			text(nodo.g.toString.take(4), nodo.x * stepX + stepX*0.1f, nodo.y * stepY + stepY*0.5f)
			text(nodo.h.toString.take(4), nodo.x * stepX + stepX*0.1f, nodo.y * stepY + stepY - textsize)
		}
	}

	def ShowValues(theValue :Int) {
		drawText= !drawText
		if(textsize>1) textSize(textsize)
	}

	def ShowSteps(theValue :Int) {showSteps= !showSteps;}

	def ShowGrid(theValue :Int) { showGrid= !showGrid;}

	def Solve {
		if(orig.x != -1 && dest.x != -1){
			clearLists
			open+=orig
			solution+=dest

			solve = true
		}
	}

	def HeuristicMultiplier(theValue :Float) { dCalc.multiplier=theValue}

	def currentSelection(theValue: Int) {currentSelection = theValue}

	def Clear{
		Maze setSize(cp5.getController("MazeSize").getArrayValue(0) toInt, cp5.getController("MazeSize").getArrayValue(1) toInt)
		stepX=(screenW-controlSize)/Maze.w.toFloat
		stepY=screenH/Maze.h.toFloat
		notWalkable.clear()
		clearLists
		orig= new Nodo(-1,-1)
		dest= new Nodo(-1,-1)
		solved = false
	}
}

object PathFinder extends PApplet{
	def main(args:Array[String]){
		PApplet.main(Array("runnables.PathFinder"))
	}
}