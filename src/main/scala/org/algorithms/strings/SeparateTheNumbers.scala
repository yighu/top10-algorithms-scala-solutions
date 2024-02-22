package org.algorithms.strings

object SeparateTheNumbers extends App {

  def separateTheNumbers(s: String): Unit = {
    var subString = ""
    var isValid = false
    for ( i <- 1 to s.length/2 if !isValid){
      subString = s.substring(0,i)
      var num = subString.toLong
      var validString = subString
      while(validString.length < s.length){
        num += 1
        validString += num.toString
      }
      if (s == validString){
        isValid = true
      }
    }
    println(if (isValid) s"Yes $subString" else "No")
  }

  val input1 = "99100101"
  val input2 = "920919"

  separateTheNumbers(input1)
  separateTheNumbers(input2)
}
