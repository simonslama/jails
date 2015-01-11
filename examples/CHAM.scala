import jails._
import jails.Process._

object CHAM_Examples {
	def main(args: Array[String]) {
		// CHAM Examples
	  // forwarding messages (defined variables scoping)
	  val ex1 = ><("y", "v") |> Printer("v") in (><("x", "u") |> <>("y", "u") in <>("x", 1))
	  //ex1.start
	  
	  // (received variable scoping)
	  val rv = ><("y", "a") |> (><("z", "v") |> Printer("v") in <>("z", "a")) in <>("y", 42)
	  //rv.start
	  
	  val ex2 = ><("x", "a") |> Printer("a") in (><("y", "u") |> <>("x", "u") in (><("x", "u") |> <>("y", "u") in <>("x", 2)))
	  //ex2.start
	  
	  val ex3 =  ><("x", "x") |> Printer("x") in (><("x1", "u") | ><("x2", "v") |> Multiplexer("u", "v") in <>("x1", 5) | <>("x2", 6))
	  //ex3.start
	  
	  
	  val laser		 = ><("laser", "x") |> Printer("x")
	  val dispatch = ><("ready", "printer") | ><("job", "file") |> <>("printer", "file")
	  val spool		 = laser in (dispatch in <>("ready", "laser") | <>("job", "datei"))
	  //spool.start
	  
	  // extended version of example 5 - without replication
	  val ex5 = (><("s") |> (Println("P"))) &
	  					(><("s") |> (Println("Q"))) in <>("s")
	  //ex5.start
	  								    
	  // extended version of example 5 - with replication
	  val ex5R = (><("s") |> (Squaller("P") | <>("s"))) &
	  					 (><("s") |> (Squaller("Q") | <>("s"))) in <>("s")
	  //ex5R.start
	  					 
	  val P = Print("P") | <>("s")
	  val Q = Print("Q") | <>("s")
	  val ex5R2 = (><("s") |> P) & 
	  						(><("s") |> Q) in <>("s")
	  					
	  //ex5R2.start						
	
	  val ex6 = ><("once") | ><("y", "v") |> Printer("v") in <>("y",1)  | <>("y",2) | <>("y",3) | <>("once")
	  //ex6.start  
	  
	  val call = <>("y",1)  | <>("y",2) | <>("y",3) | <>("once")
	  val ex6R = ><("once") | ><("y", "v") |> (Printer("v") | call) in call
	  //ex6R.start
	  
	  
	  val ex7 = ><("loop") |> (Squaller("hallo") | <>("loop")) in <>("loop") | Squaller("los")
	  //ex7.start
	}
}

case class Println(line: String) extends MyProcess {
  def body {
    println(line)
  }
}

case class Print(line: String) extends MyProcess {
  def body {
    print(line)
  }
}


case class Squaller(call: String) extends MyProcess {
	def body {
    println(call)
  }
}
  
case class Multiplexer(c1: String, c2: String) extends MyProcess {
  def body {
    dv("x") ! (rv(c1), rv(c2))
  }
}
  
