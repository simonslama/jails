import jails._
import jails.Process._

object Test {	
  def main(args: Array[String]) {

  	val hallo = (><("lydia", "x") |> Lydia("x", "simon")) &
  							(><("simon", "x") |> Simon("x", "lydia")) in <>("lydia", "hallo")

  	hallo.start
  		
  	// example how two receiver wait on the same name for the same triggered process
  	val test = ><("test", "x") | ><("test", "y") |> (Printer("x") | Printer("y")) in <>("test", 1) | <>("test", 2)
  	//test.start
  
  	//val t = ><("a", "x") | ><("b", "x") |> Print("A") in <>("y")
  }
}

case class Lydia(inputName: String, linkName: String) extends MyProcess {
	def body {
	  Thread.sleep(1000)
	  println("Lydia: " + rv(inputName))
	  dv(linkName) ! rv(inputName)
	}
}

case class Simon(inputName: String, linkName: String) extends MyProcess {
	def body {
	  Thread.sleep(1000)
	  println("Simon: " + rv(inputName))
	  dv(linkName) ! rv(inputName)
	}
}