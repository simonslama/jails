import jails._
import jails.Process._
import scala.util.Random

object Replication {

	def main(args: Array[String]) {
		// parallel replication with send
		val busy = ><("start") |> (Busy("Worker") | <>("start")) in <>("start")
		//busy.start
	
		// parallel replication with customized send
		val busy2 = ><("start") |> (Busy("Worker") | SendX("start", "", 10)) in <>("start")
		//busy2.start
		
		// Replication that works with non parallelizable processes
		// Success can only be started after the last success has been terminated
		val replication = ><("loop") |> (Success | <>("loop")) in <>("loop")
		//replication.start
		
		val parallelExample = ><("start") |> (Cnt("Candide") | Cnt("Pangloss")) in <>("start")
		//parallelExample.start
		
		val pangloss 					= Cnt("Pangloss")
		val parallelExample2 	= ><("start") |> (pangloss | pangloss) in <>("start")
		//parallelExample2.start
		
		val parallelExample3 	= ><("start") |> (pangloss | <>("start")) in <>("start")
		//parallelExample3.start
		
		val parallelPangloss 	= CntParallel("Pangloss")
		val parallelExample4 	= ><("start") |> (parallelPangloss | parallelPangloss) in <>("start")
		//parallelExample4.start
		
		val parallelExample5 	= ><("start") |> (parallelPangloss | <>("start")) in <>("start")
		parallelExample5.start
		
	}
}

case class Cnt(name: String) extends MyProcess {
  def body {
    var c 	= 0
    val rnd = new Random() 
    while(true) {
    	Thread.sleep(rnd.nextInt(500))
    	println(name + ": " + c)
    	c += 1
    }
  }
}

case class CntParallel(name: String) extends MyProcess with Parallelizable{
  var id = 0
  
  def body {
    var c 	= 0
    val rnd = new Random() 
    while(true) {
    	Thread.sleep(rnd.nextInt(500))
    	println(name + ": " + c)
    	c += 1
    }
  }
  
  def createNewInstance(): Process = {
    val instance = CntParallel(name + " " + id)
    id += 1
    instance
  }
}

case class SendX(linkName: String, message: String, limit: Integer) extends MyProcess {
  var number = 0
  
  def body {
    if(number < limit) {
      number += 1
      dv(linkName) ! message
    }
  }
}

case class Busy(x: String) extends MyProcess with Parallelizable {
  var n = 0;
  
  def body {
    var c 	= 0
    val rnd = new Random() 
    while(c < 100) {
    	Thread.sleep(rnd.nextInt(100))
    	println(x + ": " + c)
    	c += 1
    }
  }
  
  def createNewInstance: Process = { 
    n += 1
    this.copy(x = x + n)
  }
}