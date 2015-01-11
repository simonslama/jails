package jals

import scala.util.Random
import Process._
import jals.ElementaryDefintion

object Test {
val J1: JoinPattern = new Receiver("link1", "x")  //> J1  : jals.JoinPattern = Receiver(link1,x)
val J2: JoinPattern = Receiver("link2", "y")      //> J2  : jals.JoinPattern = Receiver(link2,y)
val J3: JoinPattern = ><("link3", "z")            //> J3  : jals.JoinPattern = Receiver(link3,z)

val J4: JoinPattern = ParallelJoins(J1, J2)       //> J4  : jals.JoinPattern = ParallelJoins(Receiver(link1,x),Receiver(link2,y))
val J5: JoinPattern = J4 | J3                     //> J5  : jals.JoinPattern = ParallelJoins(ParallelJoins(Receiver(link1,x),Recei
                                                  //| ver(link2,y)),Receiver(link3,z))
J5                                                //> res0: jals.JoinPattern = ParallelJoins(ParallelJoins(Receiver(link1,x),Recei
                                                  //| ver(link2,y)),Receiver(link3,z))
      
      
><("link1", "x") | ><("link2", "y") | ><("link3", "z")
                                                  //> res1: jals.JoinPattern = ParallelJoins(ParallelJoins(Receiver(link1,x),Recei
                                                  //| ver(link2,y)),Receiver(link3,z))
 
 
 val D = (><("link1", "x") |> Success) &
         (><("link2", "x") |> Success) &
         (><("link3", "x") |> Empty)              //> D  : jals.Definition = Conjunction(Conjunction(ElementaryDefintion(Receiver(
                                                  //| link1,x),jals.Success$@7b4712c3),ElementaryDefintion(Receiver(link2,x),jals.
                                                  //| Success$@7b4712c3)),ElementaryDefintion(Receiver(link3,x),jals.Empty$@7720ff
                                                  //| 6f))|
 
                    
                  
                  
}