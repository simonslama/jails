package jails

import scala.actors.Actor
import java.util.NoSuchElementException

// Mother class of the Join Pattern
sealed abstract class JoinPattern(){
  protected var belongsTo: JoinPattern = null
  protected var defines:	 ElementaryDefintion = null
  
  // needs to so the join pattern knows where it belongs to
  def setBelongsTo(whom: JoinPattern) {
    belongsTo = whom
  }
  
  // this is set when the join pattern is the top element and belongs to an elementary definition
  def setDefines(whom: ElementaryDefintion) {
    defines = whom
  }
  
  // syntactic sugar to create parallel join patterns
  def |(that: JoinPattern): JoinPattern = {
    new ParallelJoins(this, that)
  }
  
  // returns the trigger actor of this joinPattern
  def getTrigger: TriggerActor = {
    if (defines == null)
      belongsTo.getTrigger
    else
      defines.trigger
  }
  
  // class member variant of the variable getter methods
  def getDefinedVariables:  DefaultHashMapRandomValue[String, Receiver] = JoinPattern.getDefinedVariables(this)
  def getReceivedVariableNames: List[String]								 						= JoinPattern.getReceivedVariableNames(this)
  
  // starts the receive pattern and makes them wait for messages 
  def makeReadyToReceive {
    this match {
      case r: Receiver => r.start
      case ParallelJoins(join1, join2) =>
        join1.makeReadyToReceive
        join2.makeReadyToReceive
    }
  }
  
  // Syntactic Sugar
  // create elementary definition
  def |>(process: Process): ElementaryDefintion = ElementaryDefintion(this, process)
  
}

object JoinPattern {
  
  // go further down into the Join Patterns and get the defined variables df(J)
  def getDefinedVariables(pattern: JoinPattern): DefaultHashMapRandomValue[String, Receiver] = {
    pattern match {
      case ParallelJoins(join1, join2) =>
        join1.getDefinedVariables ++ join2.getDefinedVariables
      case receiver: Receiver =>
        val map = new DefaultHashMapRandomValue[String, Receiver](DefaultReceiver)
        map(receiver.linkName) = receiver 
        map
    }
  }
  
  def getReceivedVariableNames(joinPattern: JoinPattern): List[String] = {
    joinPattern match {
      case ParallelJoins(join1, join2) =>
        val rv1 = join1.getReceivedVariableNames 
        val rv2 = join2.getReceivedVariableNames
        rv1.foreach(n => if(rv2.contains(n)) throw new Exception("Variable " + n + " was defined twice in one JoinPattern"))
        rv1 ++ rv2
      case Receiver(_, varName) => List(varName)
    }
  }
}

// The other possible reception pattern are two parallel reception patterns
case class ParallelJoins(join1: JoinPattern, join2: JoinPattern) extends JoinPattern {
  join1.setBelongsTo(this)
  join2.setBelongsTo(this)
}

// Join Patterns are composed of single reception patterns
case class Receiver(linkName: String, varName: String) extends JoinPattern with Actor {
  def act() {
    loop {
      react {
        case message =>
          //println("received " + message + " over link " + linkName)
          getTrigger ! (varName, message)
      }
    }
  }
}

// Companion Object Receiver is used as a Default Receiver - 
// This receiver is predefined in every Sending Process.
// It is an actor that prints every received message - no matter what
// If it outputs a message it is because no receiver was defined.
object DefaultReceiver extends Receiver("DefaultReceiver", "x"){
  this.start
	override def act() {
	  loop {
      react {
        case whatever =>
          throw new NoSuchElementException("Proper Receiver was not defined. Message: [" + whatever + "] Type: [" + whatever.getClass + "]")
      } 
    }
  }  
}