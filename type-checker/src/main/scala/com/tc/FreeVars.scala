
package com.tc

class FreeVar {
  
  def get(): String = {
    "X" + FreeVar.getCounter()
  }
}

object FreeVar {

  private[this] var counter:Int = 0 
  
  def apply() = {
    counter += 1
    new FreeVar
  }
  
  def getCounter() = counter  // accessor method
}