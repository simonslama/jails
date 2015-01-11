import jails._
import jails.Process._

object Factorial extends MyProcess {
	
	def main(args: Array[String]) {
		val fak = (><("fac", "n") 	| ><("cur", "c") |> Factorial) &
							(><("print", "x") 					       |> Printer("x")  ) in <>("fac", 5) | <>("cur", 1)
		fak.start
	}

  def body {
		val n = rv[Integer]("n")
		val c = rv[Integer]("c")
		  
		if(n == 1) 
		  dv("print") ! c * n
		else
		  dv("fac") ! n - 1
		  dv("cur") ! n * c
		}
}