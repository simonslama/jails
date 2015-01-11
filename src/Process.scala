package jails

import scala.actors.Actor
import scala.actors.Actor.State


// Companion Object of the Process class
object Process {
  
  // Syntactic Sugar for sending and receiving messages
  def <>(linkName: String, message: Any    = ""): Sender   = Sender(linkName, message)
  def ><(linkName: String, varName: String = ""): Receiver = Receiver(linkName, varName)  
  
}

// Mother class of the process hierarchy
abstract class Process extends Actor{
 
  // The default definition (interpreted as a non definition)
  var definedVariables = new DefaultHashMapRandomValue[String, Receiver](DefaultReceiver)
  var receivedVariables = new DefaultHashMap[String, Any](null) 
  
  def setDefinedVariables(definedVariables: DefaultHashMapRandomValue[String, Receiver]) { 
    this match {
      case parallel: Parallel =>
        parallel.process1.setDefinedVariables(definedVariables)
        parallel.process2.setDefinedVariables(definedVariables)
        parallel.definedVariables = definedVariables
      case process => 
        process.definedVariables = definedVariables 
    }
  }
  
  def setReceivedVariables(receivedVariables: DefaultHashMap[String, Any]) { 
    this match {
      case parallel: Parallel =>
        parallel.process1.setReceivedVariables(receivedVariables)
        parallel.process2.setReceivedVariables(receivedVariables)
        parallel.receivedVariables  = receivedVariables
      case process => 
        process.receivedVariables = receivedVariables
      	
    }
  }
  
  // Syntactic sugar to create processes 
  // parallel processes
	def |(that: Process): Process =
	  new Parallel(this, that)
	
	def act() {
	  // Nothing so far
	}
	
	override def start: Actor = {
    this.getState match {
      case State.New => super.start
      case _ =>
        while(this.getState != State.Terminated) {
        	Thread.sleep(10)
        }
        super.restart
        this
    }
	}
}

trait Parallelizable {
  def createNewInstance: Process
  def getMaximumNumberOfParallelInstances : Integer = 0
  def getCurrentNumberOfRunningInstances  : Integer = 0 
}

abstract class MyProcess extends Process{
  override def act() {
    body
  }
  
  final protected def dv(linkName: String): Receiver = {
    definedVariables(linkName)
  }

  final protected def rv[A](varName: String): A = {
    receivedVariables(varName).asInstanceOf[A]
  } 
  
  def body: Unit
}

// Parallel processes
case class Parallel(process1: Process, process2: Process) extends Process {
  override def act() {
    
    var p1 = process1
    var p2 = process2
    
    // make the current definition available to the subprocesses
    process1.setDefinedVariables(definedVariables)
    process2.setDefinedVariables(definedVariables)
    process1.setReceivedVariables(receivedVariables)
    process2.setReceivedVariables(receivedVariables)

    
    // check if the process should be started as a new instance (created by copy function) or the original
    process1 match { 
      case p: Parallelizable => 
        p1 = p.createNewInstance
        p1.setDefinedVariables(definedVariables)
        p1.setReceivedVariables(receivedVariables)
      case _ =>
    }
    process2 match { 
      case p: Parallelizable => 
        p2 = p.createNewInstance
        p2.setDefinedVariables(definedVariables)
        p2.setReceivedVariables(receivedVariables)
      case _ =>
    }   
    
    // start the subprocesses
    p1.start
    p2.start
  }
}

// Asynchronous sending
// At the moment the send pattern is started it should be provided with a Map of definitions that are
// available in this right scope
case class Sender(linkName: String, value: Any) extends Process {
  override def act() {
    
    var message = value
    var link		= definedVariables(linkName)
    
    // if the value is known from the received variables as a variable name
    // pass the content on to the receiver
    if(receivedVariables(value.toString) != null)
    	message = receivedVariables(value.toString)
    
    // if the linkName is known from the received variables as a link in the defined variables use this link instead
    if(receivedVariables(linkName) != null)
      link = definedVariables(receivedVariables(linkName).toString)    
      
    //println("Sending " + value + " over link " + linkName )
    link ! message
  }
}

// A definition "Def D in P" that is composed of a Definition
// and a process the definition is available
case class DefinitionInProcess(definition: Definition, process: Process) extends Process {  
  override def act() {
    // start the underlying reception pattern
    definition.makeReadyToReceive
    
    // get the variable definitions
    // at this point defined variables have to be overwritten if already defined
    val newDefinedVariables = definedVariables + definition.getDefinedVariables 
    
    // make them public to the process the definition includes
    process.setDefinedVariables(newDefinedVariables)
    process.setReceivedVariables(receivedVariables)
    
    // and make them also available for the triggered processes
    // The received variables are sent via the trigger actor
    // Name clashes are handled in there
    definition.makeVariablesAvailableForTriggeredProcesses(newDefinedVariables)
    
    // check if the process should be started as a new instance (created by copy function) or the original
    process match {
      case p: Parallelizable =>
        val instance = p.createNewInstance
        instance.start
      case _ =>
        process.start
    }  
  }
}

// 2 Objects that represent the empty and the success Process
object Empty extends Process {
  override def act() {
  }
}

object Success extends Process {
  override def act() {
    println("success")
  }
}
