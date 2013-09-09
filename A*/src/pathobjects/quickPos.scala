package pathobjects

import controlP5.{Button, ControllerInterface, Controller}
import processing.core.PVector

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 21/08/13
 * Time: 01:37
 * To change this template use File | Settings | File Templates.
 */
object quickPos {
	var initPos = new PVector(0,0)
	var pos = new PVector(0,0)
	var padding = new PVector(10,10)

	def initQuickPos(xPos:Float, yPos:Float){
		initPos.x=xPos
		initPos.y=yPos
		pos.x=xPos
		pos.y=yPos
	}

	def nextQuickItem(xDist:Float, yDist:Float, controler: ControllerInterface[_]){
		controler.setPosition(pos.x, pos.y)
		pos.x+=padding.x+controler.getWidth+xDist
	}

	def nextQuickItem(controler: ControllerInterface[_]){
		controler.setPosition(pos.x, pos.y)
		pos.x+=padding.x+controler.getWidth
	}

	def linebreakQuickItem(xDist:Float, yDist:Float,controler: ControllerInterface[_]){
		controler.setPosition(pos.x, pos.y)
		pos.x=initPos.x+xDist
		pos.y+=padding.y+controler.getHeight+yDist
	}

	def linebreakQuickItem(controler: ControllerInterface[_]){
		controler.setPosition(pos.x, pos.y)
		pos.x=initPos.x
		pos.y+=padding.y+controler.getHeight
	}
}
