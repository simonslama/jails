import jails._
import jails.Process._

object Test {	
  def main(args: Array[String]) {

  	val hallo = (><("achim", "x") |> Achim("x", "dirk")) &
  							(><("dirk", "x") |> Dirk("x", "achim")) in <>("achim", "hallo")

  	hallo.start
  		
  	// example how two receiver wait on the same name for the same triggered process
  	val test = ><("test", "x") | ><("test", "y") |> (Printer("x") | Printer("y")) in <>("test", 1) | <>("test", 2)
  	//test.start
  
  	//val t = ><("a", "x") | ><("b", "x") |> Print("A") in <>("y")
  }
}

case class Achim(inputName: String, linkName: String) extends MyProcess {
	def body {
	  Thread.sleep(1000)
	  println("Achim: " + rv(inputName) + " " + linkName)
	  dv(linkName) ! rv(inputName)
	}
}

case class Dirk(inputName: String, linkName: String) extends MyProcess {
	def body {
	  Thread.sleep(1000)
	  println("Dirk: " + rv(inputName) + " " + linkName)
	  dv(linkName) ! rv(inputName)
	}
}