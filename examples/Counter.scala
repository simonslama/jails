import jails._
import jails.Process._

object Counter {
	def main(args: Array[String]) {
		val counter = ><("count", "x") |> (Incrementor("x", "count") | Printer("x")) in <>("count", 1)
		counter.start
	}
}
  
case class Incrementor(inputName: String, outputLinkName: String ) extends MyProcess {
	def body {
  	val input = rv[Integer](inputName)
  	val link	= dv(outputLinkName)
  	
  	Thread.sleep(1000)
  	link ! input + 1
  }
}
	
case class Printer(inputName: String) extends MyProcess {
  def body {
    println(rv(inputName))
  }
}