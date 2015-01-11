package jails

import scala.util.Random
import scala.collection.mutable.HashMap

class Util {
  
}

class DefaultHashMap[A, B](defaultValue: B) extends HashMap[A, B] {
	override def default(key: A) = defaultValue
	
	// returns the union of the map. values are overwritten by the second map if there are duplicate keys
	def ++(that: DefaultHashMap[A, B]): DefaultHashMap[A, B] = {
	  val map = new DefaultHashMap[A, B](this.defaultValue)
	  
	  // This will overwrite key -> set pairs with the new ones
	  // As in J|J there are only different names so this works fine
	  this.foreach { case(key, value) => map(key) = value }
	  that.foreach { case(key, value) => map(key) = value }
	  
	  map
	}
}

// For the defined maps a more powerfull map is needed
// There can be more than one receiver that waits for messages on one channel
// This map collects them in a list. When accessed it returns one randomly
class DefaultHashMapRandomValue[A, B](defaultValue: B) {
  // internal hashmap that stores the data
  var internMap = new HashMap[A, List[B]]
  
  // joins a map so that doubled keys means that their value lists are joined as well
  def ++(that: DefaultHashMapRandomValue[A, B]): DefaultHashMapRandomValue[A, B] = {
    var resultMap = new DefaultHashMapRandomValue[A, B](defaultValue)
      
    internMap.foreach { case(key, value) => resultMap.internMap(key) = value }
    that.internMap.foreach {
      case(key, value) =>
        if(resultMap.internMap.contains(key))
          resultMap.internMap(key) ++= value
        else
          resultMap.internMap(key) = value
    }
    
    resultMap
  }
  
  // joins a map so that already existing keys are overwritten with the new one
  def +(that: DefaultHashMapRandomValue[A, B]): DefaultHashMapRandomValue[A, B] = {
    var resultMap = new DefaultHashMapRandomValue[A, B](defaultValue)
      
    internMap.foreach { case(key, value) => resultMap.internMap(key) = value }
    that.internMap.foreach { case(key, value) => resultMap.internMap(key) = value }

    resultMap
  }
  
  // get value for key
  // if there are more than one in the list, pick one randomly
  def apply(key: A): B = {
    if(!internMap.contains(key)) { 
      defaultValue
    }
    else {
    	val rand = new Random()
    	internMap(key)(rand.nextInt(internMap(key).size))
    }
  }
  
  // update value for key or insert if it does not exists
  // if it already exists add the value to the map
  def update(key: A, value: B) = {
    if(!internMap.contains(key)) {
      internMap(key) = List(value)
    }
    else {
      internMap(key) = internMap(key).+:(value)
    }
  }
  
  override def toString: String = internMap.toString
}