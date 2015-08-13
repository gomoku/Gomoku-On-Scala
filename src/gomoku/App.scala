package gomoku

import swing._
import swing.event._
import java.awt.{Graphics2D, Point, Cursor}

object App extends SimpleSwingApplication {
	def top = new MainFrame {
		title = "Gomoku"
		resizable = false
		
		menuBar = new MenuBar {
			contents += newGame
			contents += goStep
			contents += goRun
			contents += goBack
		}
		
		listenTo(newGame, goStep, goRun, goBack)
		
		reactions += {
			case ButtonClicked(`newGame`) => Game.play
			case ButtonClicked(`goStep`) => Game.run(false)
			case ButtonClicked(`goRun`) => Game.run(true)
			case ButtonClicked(`goBack`) => Game.back
		}

		//Net.init
		//Desk.init
		//Play.init
		
		contents = new BoxPanel(Orientation.Vertical) {
			contents += Desk
			contents += label
		}
		
	}
	
	lazy val newGame = new Button("New") {enabled = true}
	lazy val goStep = new Button("Step") {enabled = false}
	lazy val goRun = new Button("Run")	 {enabled = false}
	lazy val goBack = new Button("Back") {enabled = false}
	
	lazy val label = new Label("Ready")
	
	def status(t: String) = label.text = t 
}

object Desk extends Panel{
	
	class St(val x: Byte, val y: Byte, val mes: String)
	
	val desk_size = 500
	val shift = 20
	val l = desk_size - 2 * shift
	val d = l / 16 + 1
	val pi = Math.Pi
	
	val c = Array.ofDim[Int](15)
	val color = Array(0, 255)
	val steps = Array.ofDim[St](255)
	
	var qsteps = 0
		
	preferredSize = new Dimension(desk_size, desk_size)
	background = new Color(128, 128,128)
	
	listenTo(mouse.moves, mouse.clicks)
	
	reactions += {
		case MouseMoved(src, point, mod) => set_cursor(point)
		case MouseClicked(src, point, mod, n, t) => set_step(point)
	}
	
	def init = {
		qsteps = 0
		add_step(0, 0, "Start")
	}
	
	def set_cursor(p: Point) = {
		lazy val x = get_dc(p.x)
		lazy val y = get_dc(p.y)
		
		if(Game.is_play && !Game.is_run && !Game.is_busy && 
				x != -9 && y != -9 && 
				Net.get_point(x, y).s == 0) 
			cursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR)
		else
			cursor = Cursor.getDefaultCursor
	}
	
	def set_step(p: Point) = {
		lazy val x = get_dc(p.x)
		lazy val y = get_dc(p.y)
		
		if(Game.is_play && !Game.is_run && !Game.is_busy &&
				x != -9 && y != -9 && 
				Net.get_point(x, y).s == 0) 
			Game.go(false, x, y)
	}
	
	def get_dc(mc: Int): Byte = {
          for(i <- 0 until 15)
            if(mc < c(i) + d / 2 && mc > c(i) - d / 2) 
                return (i - 7).toByte
        
          -9
        }
	
	def redraw = repaint
		
	override def paintComponent(g: Graphics2D): Unit = {
		super.paintComponent(g)
		render(g)
	}
	
	def add_step(x: Byte, y: Byte, mes: String) = {
		steps(qsteps) = new St(x, y, mes)
		qsteps += 1	
	}
	
	def render(g: Graphics2D): Unit = {
		
		square
		c(7) = l / 2 + shift
		vline(c(7))
        hline(c(7))
		for(i <- 1 to 7) {
			c(7 - i) = c(7) - d * i
			c(7 + i) = c(7) + d * i
			vline(c(7 + i))
			vline(c(7 - i))
			hline(c(7 + i))
			hline(c(7 - i))
		}
	
		show_steps
		
		def show_steps = for(i <- 0 until qsteps) show_step(steps(i), i)
				
		def show_step(s: St, i: Int) = put(s.x, s.y, i, i % 2 * 255)
				
		def put(x: Byte, y: Byte, n: Int, c: Int) = 
			circle(get_c(x), get_c(y), n, new Color(c, c, c), new Color(Math.abs(c - 255), Math.abs(c - 255), Math.abs(c - 255)))
				
		def get_c(i: Int): Int = c(7 + i)

		def circle(x: Int, y: Int, n: Int, c: Color, t: Color) = {
			val s: String = (n + 1).toString
			val w: Int = (3 - s.length) * d/6 + d/12
			
			g.setColor(c)
			g.fillOval(x - d / 2, y - d / 2, d, d)
			
			g.setColor(new Color(0, 0, 0))
			g.drawOval(x - d / 2, y - d / 2, d, d)
			
			g.setColor(t)
			g.drawString(s, x - d / 2 + w, y + d/5)
		}
		
		def square = {
			g.setColor(new Color(0, 0, 0))
			g.drawRect(shift, shift, l, l)
		}
		
		def vline(x: Int) = {
			g.setColor(new Color(0, 0, 0))
			g.drawLine(x, shift, x, l + shift)
		}

		def hline(y: Int) = {
			g.setColor(new Color(0, 0, 0))
			g.drawLine(shift, y, l + shift, y)
		}
	}
}

