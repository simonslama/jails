import jails._
import jails.Process._
import scala.util.Random

object MCPi {
	def main(args: Array[String]) {
	  
	  // The Process, that collects the data
	  val collector = monteCarloPi("v")
  
	  // The subprocess that creates the random points cannot run parallel
	  // the next instance will run after the first one stopped
	  val mcPi = (><("collector", "v") |> collector) &
	  					 (><("print", "x")		 |> Printer("x")) &
	  					 (><("calculatePi") 	 |> (<>("calculatePi") |  CreateRandomPoints("collector") )) in <>("calculatePi")
	
	  					 
	  // This one on the other hand is able to be cloned, so it can be run parallel					 
	  val randomPointSource = CreateRandomPointsParallel("collector", 1)					 
	  					 
	  val mcPi2 = (><("collector", "v") |> collector) &
	  					  (><("print", "x")		  |> Printer("x")) &
	  					  (><("calculatePi") 	  |> randomPointSource) &
	  					  (><("loop")					  |> (LimitedReplicator("calculatePi", "", randomPointSource ) | <>("loop"))) in <>("loop")
	  
	  					 
	  collector.init
	  /*
	  if(args(0).equals("1"))
	  	mcPi.run  					 
	  if(args(0).equals("2"))
	  {
	  	mcPi2.run
	  }
	  	*/
	  
	  mcPi.start	
	  
	}
}

case class LimitedReplicator(outputChannel: String, inputValue: String, process: Parallelizable) extends MyProcess {
  def body {
    val maximumNumber = process.getMaximumNumberOfParallelInstances
    val currentNumber = process.getCurrentNumberOfRunningInstances
    
    if(currentNumber < maximumNumber) {
      dv(outputChannel) ! rv(inputValue)
    }
  }
}

case class CreateRandomPoints(outputChannel: String, var id: Integer = 0) extends MyProcess {
	def body {
   	val createStart = System.nanoTime

		//println("Creator " + id + " started")
		val rnd = new Random()
		var result = Set[(Double, Double)]()
   
		for(i <- 0 to 100000)
		{
			// create two random points
			val x = rnd.nextDouble() * 2 - 1 
			val y = rnd.nextDouble() * 2 - 1
			result += ((x, y))
    }

		//println("Creator " + id + " finished")
		this.id += 1
		dv(outputChannel) ! result
		
    //println((System.nanoTime - createStart) / 1000000.0)
  }
}

case class CreateRandomPointsParallel(outputChannel: String, limit: Integer = 0, id: Integer = 0) extends MyProcess with Parallelizable{
  var n 							  = 0
  var currentlyRunnning = 0
  var original: CreateRandomPointsParallel = this

  def body {
  	val createStart = System.nanoTime

		original.currentlyRunnning += 1
		//println("Creator " + id + " started")
		val rnd = new Random()
		var inside 	= 0
		var outside = 0
		var result = Set[(Double, Double)]()
		
		for(i <- 0 to 100000)
		{
			// create two random points
			val x = rnd.nextDouble() * 2 - 1 
			val y = rnd.nextDouble() * 2 - 1
			result += ((x, y))
		}
		
		dv(outputChannel) ! result
		//println("Creator " + id + " finished")
		original.currentlyRunnning -= 1
    //println((System.nanoTime - createStart) / 1000000.0)
  }
  
	def createNewInstance =  {

	  n += 1
    //val instance = this.copy(id = n)
    val instance = CreateRandomPointsParallel(outputChannel, limit, n)
	  
    var current = this
    while(current.original != current)
      current = current.original
      
    instance.original = current
    instance
  }
	
	override def getMaximumNumberOfParallelInstances : Integer = limit
	override def getCurrentNumberOfRunningInstances  : Integer = currentlyRunnning   
}

case class monteCarloPi(inputChannel: String) extends MyProcess {
  var inside  						= 0
  var outside 						= 0
  var n									  = 0
  var numberOfHitsInARow  = 0
  var lastValue: Double		= 0
  var startTime: Long			= 0
  var runtimeStart: Long  = 0
  var runtime: Long				= 0
  
  def init {
    this.startTime = System.nanoTime
    println("initialized")
  }
  def body {
    //println("Collector started")
    runtimeStart = System.nanoTime
    n += 1
    val points = rv[Set[(Double, Double)]](inputChannel) 
    points.foreach { 
      case (x, y) => 
        val d = Math.sqrt((x * x + y * y))
        if (d <= 1.0) inside  += 1
        else					outside += 1

        val pi = inside * 1.0 / (outside + inside) * 4
    
	      if(isCloseEnough(pi, lastValue)) {
	        numberOfHitsInARow += 1
	        
	        if(numberOfHitsInARow == 10000) {
	          runtime += System.nanoTime() - runtimeStart
	          val time = (System.nanoTime - startTime)
	          println("pi = " + pi)
	          println("time = " + (time / 1000000000.0))
	        	println("runtime = " + (runtime / 1000000000.0))
	        	println("idle = " + ((time - runtime) / 1000000000.0))
	          println("number = " + (inside  + outside) )
	        	System.exit(0)
	        }
	      }
	      else numberOfHitsInARow = 0
	      
	      lastValue = pi
    }
    runtime += System.nanoTime() - runtimeStart 
  }
  
  def isCloseEnough(v1: Double, v2: Double): Boolean = Math.abs(v1 - v2) < 0.0000001
}
