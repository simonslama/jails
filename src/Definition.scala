package jails

// Companion Object of the Definition
object Definition {
  // create a map of the name of an link and the link itself
  // go down into the Definitions
  def getDefinedVariables(definition: Definition): DefaultHashMapRandomValue[String, Receiver] = {
    definition match {
      case Conjunction(definition1, definition2) =>
    		definition1.getDefinedVariables ++ definition2.getDefinedVariables
      case ElementaryDefintion(pattern, _) =>
      	pattern.getDefinedVariables
    }
  }
}

// Mother class of Definition
sealed abstract class Definition {
  def &(that: Definition): Definition =
    new Conjunction(this, that)
  
  // TODO: the varMap should not really change during the execution...
  // create caching
  
  // Use the getVarMap function of the companion object
  def getDefinedVariables: DefaultHashMapRandomValue[String, Receiver] = Definition.getDefinedVariables(this)
  
  // a triggered process can also send messages to the links defined by this definition
  def makeVariablesAvailableForTriggeredProcesses(vars: DefaultHashMapRandomValue[String, Receiver]) {
    this match {
      case ed: ElementaryDefintion =>
        ed.triggeredProcess.setDefinedVariables(vars)
      case Conjunction(def1, def2) =>
        def1.makeVariablesAvailableForTriggeredProcesses(vars)
        def2.makeVariablesAvailableForTriggeredProcesses(vars)
    }
  }
  
  // makes the reception pattern ready to receive messages
  def makeReadyToReceive {
    this match {
      case ElementaryDefintion(pattern, _) => pattern.makeReadyToReceive
      case Conjunction(def1, def2) => 
        def1.makeReadyToReceive
        def2.makeReadyToReceive
    }
  }
  
  // Syntactic Sugar
  // create Definition in Progress
  def in(process: Process) : DefinitionInProcess = DefinitionInProcess(this, process)
}

// Definition can be an elementary Definition that is composed of a JoinPattern and the triggered Process 
case class ElementaryDefintion(pattern: JoinPattern, process: Process) extends Definition {
  
	// check if the process should be started as a new instance (created by copy function) or the original
  var triggeredProcess = process match {
   	case p: Parallelizable =>
   		p.createNewInstance
   	case _ =>
   	  process
  }
  
  // setting the trigger actor and starting it
  val trigger = new TriggerActor(pattern.getReceivedVariableNames, triggeredProcess)
  trigger.start
  
  // the pattern that compounds this elementary definition needs to know this so it can call the trigger
  pattern.setDefines(this)
}

// Definition can be several pairs of Pattern and Process
case class Conjunction(definition1: Definition, definition2: Definition) extends Definition

