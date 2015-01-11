package jails

import scala.actors.Actor
import scala.collection.mutable.Queue
import scala.collection.mutable.HashMap

// Class that manages the reception pattern
// checks if all the join pattern were satisfied with a message
// if so => trigger the process
case class TriggerActor(receivedVariableNames: List[String], process: Process) extends Actor {
  val messageQueueMap = HashMap[String, Queue[Any]]()
  receivedVariableNames.foreach(n => messageQueueMap(n) = Queue[Any]())
  
  def act {
    loop {
      react {
        case (varName: String, value: Any) =>
          // adding new message to the specified queue
          messageQueueMap(varName) += value
          
          // checking if the pattern is complete
          var complete = messageQueueMap.values.foldLeft[Boolean](true)((b, q) => b && !q.isEmpty)
          
          if (complete) {
            
            var triggeredProcess = process
            
            // create receivedVarMap for the process
            val varMap = new DefaultHashMap[String, Any](null)
            for((k,v) <- messageQueueMap) varMap(k) = v.dequeue
            
            process.setReceivedVariables(varMap)
 
            // if this process is a parallelizable process it has to be instanciated first
            process match { 
            	case p: Parallelizable => 
            		triggeredProcess = p.createNewInstance
            		triggeredProcess.setDefinedVariables(process.definedVariables)
            		triggeredProcess.setReceivedVariables(process.receivedVariables)
            	case _ =>
            }
            
            triggeredProcess.start
          }
      }
    }
  }
}