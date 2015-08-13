package gomoku

import collection.mutable._

object Game {
  var is_busy = false
  var is_play = false
  var is_run = false
   
  def play = {
    App.status("New game")
    //App.newGame.enabled = false
    App.goRun.enabled = true
    App.goStep.enabled = true
    App.goBack.enabled = false
      
    Net.init
    Desk.init
    Play.init
      
    is_busy = false
    is_play = true
    is_run = false
      
    Desk.redraw
      
  }
    
  def run(r:  Boolean) =
    if(is_play) {
      App.status("Thinking...")
      is_busy = true
      is_run = r
      go(true, 0, 0)
      is_busy = false
    }
    
  def back() = {
    is_play = true
    is_busy = true
    replay(1)
    is_busy = false
        
    App.goRun.enabled = true
    App.goStep.enabled = true
  }
    
  def go(auto: Boolean, x: Byte, y: Byte): Unit = {
		
    val ret = if(auto) Play.next_step() else Play.manual_step(x: Byte, y: Byte)

    App.status(if(ret < 0) "Finish! -> " +  Play.mes else "Step " + ret + " -> " + Play.mes)

    Desk.redraw
		
    App.goBack.enabled = true

    if(ret < 0 || ret > 224) {
      //App.newGame.enabled = true
      App.goRun.enabled = false
      App.goStep.enabled = false

      is_run = false
      is_play = false
    }
    else if(is_run) go(true, 0, 0)
		
  }
	
  def replay(n: Int): Unit = {
    lazy val n_step = Play.n_step - n
		
    if(n_step > 0) {
      Net.init
      Desk.init
      Play.init
      
      is_busy = false
      is_play = true
      is_run = false
			
      App.status("Start")
      Desk.redraw
    }
		
    if(n_step > 1) {
			
      var st: Desk.St = null
			
      for(i <- 1 until n_step) {
        st = Desk.steps(i)
        Desk.steps(i) = null
        Play.replay_step(st.x, st.y, st.mes)
      }

      App.status("Step " + n_step + " -> " + st.mes)
      Desk.redraw
    }
  }
}

object Play {

  var n_step: Int = 0
  var mes: String = ""
	
  def init = {
    mes = "Start"
    n_step = 1
    Net.step(0, 0, 1)
  }
	
  def name_c(c: Byte): String =
    if(c == 1) "b"
    else "w"
	
  def next_step(): Int = {
    n_step += 1
		
    if(check_win((3 - (2 - n_step%2)).toByte) || check_draw()) -1
    else {
      val p: Net.Point = calc_point((2 - n_step%2).toByte).get
      Net.step(p.x, p.y, (2 - n_step%2).toByte)
      Desk.add_step(p.x, p.y, mes)
      n_step
    }
  }
	
  def manual_step(x: Byte, y: Byte): Int = {
    n_step += 1
		
    if(check_win((3 - (2 - n_step%2)).toByte) || check_draw()) -1
    else {
      Net.step(x, y, (2 - n_step%2).toByte)
      mes = name_c((2 - n_step%2).toByte) + " :: manual (" + x + ":" + y + ")"
      Desk.add_step(x, y, mes)
      n_step
    }
  }
	
  def replay_step(x: Byte, y: Byte, mes: String): Int = {
    n_step += 1
    Net.step(x, y, (2 - n_step%2).toByte)
    Desk.add_step(x, y, mes)
    n_step
  }
	
  def check_win(c: Byte): Boolean = {
    Net.active_slots(c).foreach(s =>
      if (s.r == 5) {
        mes = name_c(c) + " :: win!!!"
        return true
      }
    )
    false
  }
	
  def check_draw(): Boolean =
    if(Net.active_slots(0).length == 0 &&
       Net.active_slots(1).length == 0 &&
       Net.active_slots(2).length == 0) {
      mes = " draw :("
      true
    }
    else false
	
  def calc_point(c: Byte): Option[Net.Point] = {
    var ret: Option[Net.Point] = None
		
    mes = name_c(c) + " :: auto :: "
		
    ret = find_slot_4(c)
    if(ret == None) ret = find_slot_4((3 - c).toByte)
		
    if(ret == None) ret = find_point_x(c, 2, 1)
    if(ret == None) ret = find_point_x((3 - c).toByte, 2, 1)
        
    if(ret == None) ret = find_point_x(c, 1, 5)
    if(ret == None) ret = find_point_x((3 - c).toByte, 1, 5)
        
    if(ret == None) ret = find_point_x(c, 1, 4)
    if(ret == None) ret = find_point_x((3 - c).toByte, 1, 4)
        
    if(ret == None) ret = find_point_x(c, 1, 3)
    if(ret == None) ret = find_point_x((3 - c).toByte, 1, 3)
        
    if(ret == None) ret = find_point_x(c, 1, 2)
    if(ret == None) ret = find_point_x((3 - c).toByte, 1, 2)
        
    if(ret == None) ret = find_point_x(c, 1, 1)
    if(ret == None) ret = find_point_x((3 - c).toByte, 1, 1)
        
    if(ret == None) ret = find_point_x(c, 0, 10)
    if(ret == None) ret = find_point_x((3 - c).toByte, 0, 10)
		
    if(ret == None) ret = find_point_x(c, 0, 9)
    if(ret == None) ret = find_point_x((3 - c).toByte, 0, 9)
        
    if(ret == None) ret = find_point_x(c, 0, 8)
    if(ret == None) ret = find_point_x((3 - c).toByte, 0, 8)
        
    if(ret == None) ret = find_point_x(c, 0, 7)
    if(ret == None) ret = find_point_x((3 - c).toByte, 0, 7)
               
        
    if(ret == None) ret = calc_point_max_rate(c)
            
    ret
  }
	
  def find_slot_4(c: Byte): Option[Net.Point] = {
           
    Net.active_slots(c).foreach(s =>
      if (s.r == 4)
        s.points.foreach(p =>
          if(p.s == 0) {
            mes += name_c(c) + " :: find_slot_4 -> (" + p.x + ":" + p.y + ")"
            return Some(p)
          }
        )
    )
        
    None
  }
	
  def find_point_x(c: Byte, r: Byte, b: Byte): Option[Net.Point] = {
            
    Net.empty_points.foreach(p => {
        var i = 0
        p.slots.foreach(s =>
          if(s.s == c && s.r > r) i += 1
        )
        if(i > b) {
          mes += name_c(c) + " :: find_point_x(" + r + ", " + b + ") -> (" + p.x + ":" + p.y + ")"
          return Some(p)
        }
      })
        
    None
  }
	
  def calc_point_max_rate(c: Byte): Option[Net.Point] = {
    var ret: Option[Net.Point] = None
            
    var r = -1f
    var i = 0
    var d = 0f
    Net.empty_points.foreach(p => {
        i += 1
        d = 0f
        p.slots.foreach(s =>
          if(s.s == 0) d += 1f
          else if(s.s == c) d += (1f + s.r) * (1f + s.r) //+ 0.5f
          else if(s.s != 3) d += (1f + s.r) * (1f + s.r)
        )
        if(d > r) {
          r = d
          ret = Some(p)
        }
      })
 
    mes += name_c(c) + " :: point_max_rate(" + i + ", " + r + ") -> (" + ret.get.x + ":" + ret.get.y + ")"
    ret
  }
}

object Net {
  val N: Int = 225
  val F: Int = 5
	
  val all_points = Array.ofDim[Point](15, 15)
  val all_slots = ArrayBuffer.empty[Slot]
	
  val empty_points = ArrayBuffer.empty[Point]
  val active_slots = Array.ofDim[ArrayBuffer[Slot]](3)
	
  def init = {
		
    active_slots(0) = ArrayBuffer.empty[Slot] // free
    active_slots(1) = ArrayBuffer.empty[Slot] // black
    active_slots(2) = ArrayBuffer.empty[Slot] // white
		
    empty_points.clear
    all_slots.clear
		
    for (i <- 0 until 15; j <-0 until 15) {
      val p = new Point((i -7).toByte, (j - 7).toByte)
      all_points(i)(j) = p
      empty_points += p
			
      for(l <-0 until 4)
        if (p.is_valid_scp(l.toByte)) {
          val s = new Slot(p, l.toByte)
          all_slots += s
          active_slots(0) += s
        }
    }
		
    all_slots.foreach(_.init)
  }
	
  def step(x:Byte, y:Byte, c:Byte) = {
    val p = get_point(x, y)
    p.s = c
    empty_points -= p
		
    p.slots.foreach(s =>
      if(s.s == 0) {
        p.r(0) = (p.r(0) - 1).toByte
        p.r(c) = (p.r(c) + 1).toByte
        s.s = c
        s.r = 1
        active_slots(0) -= s
        active_slots(c) += s
      }
      else if(s.s == c) {
        p.r(c) = (p.r(c) + 1).toByte
        s.r = (s.r + 1).toByte
      }
      else if(s.s != 3) {
        p.r(c) = (p.r(c) - 1).toByte
        active_slots(s.s) -= s
        s.s = 3
      }
    )
  }
	
  def get_point(x: Byte, y: Byte): Point = all_points(x + 7)(y + 7)
		
  class Slot(val scp: Point, val d: Byte) {
    val points = Array.ofDim[Point](F)
    var r: Byte = 0
    var s: Byte = 0
		
    def init = {
						
      points(2) = Net.get_point(scp.x, scp.y)
			
      d match {
        case 0 => {
            points(0) = Net.get_point(scp.x, (scp.y - 2).toByte)
            points(1) = Net.get_point(scp.x, (scp.y - 1).toByte)
            points(3) = Net.get_point(scp.x, (scp.y + 1).toByte)
            points(4) = Net.get_point(scp.x, (scp.y + 2).toByte)
          }
        case 1 => {
            points(0) = Net.get_point((scp.x - 2).toByte, scp.y)
            points(1) = Net.get_point((scp.x - 1).toByte, scp.y)
            points(3) = Net.get_point((scp.x + 1).toByte, scp.y)
            points(4) = Net.get_point((scp.x + 2).toByte, scp.y)
          }
        case 2 => {
            points(0) = Net.get_point((scp.x - 2).toByte, (scp.y - 2).toByte)
            points(1) = Net.get_point((scp.x - 1).toByte, (scp.y - 1).toByte)
            points(3) = Net.get_point((scp.x + 1).toByte, (scp.y + 1).toByte)
            points(4) = Net.get_point((scp.x + 2).toByte, (scp.y + 2).toByte)
          }
        case 3 => {
            points(0) = Net.get_point((scp.x - 2).toByte, (scp.y + 2).toByte)
            points(1) = Net.get_point((scp.x - 1).toByte, (scp.y + 1).toByte)
            points(3) = Net.get_point((scp.x + 1).toByte, (scp.y - 1).toByte)
            points(4) = Net.get_point((scp.x + 2).toByte, (scp.y - 2).toByte)
          }
      }
			
      for(i <- 0 until 5) points(i).add_slot(this);
                
    }
		
  }
	
  class Point(val x: Byte, val y:Byte) {
    val slots = ArrayBuffer.empty[Slot]
    val r = Array[Byte](0, 0, 0)
    var s:Byte = 0
		
    def add_slot(s: Slot) = {
      slots += s
      r(s.s) = (r(s.s) + 1).toByte
    }
		
    def is_valid_scp(d: Byte): Boolean = d match { //// 0 - vert, 1 - horiz, 2 - up, 3 - down
      case 0 if (y > -6 && y < 6) => true
      case 1 if (x > -6 && x < 6) => true
      case 2 if ((x > -6 && y < 6) && (x < 6 && y > -6)) => true
      case 3 if ((x > -6 && y > -6) && (x < 6 && y < 6)) => true
      case _ => false
    }
  }
}

