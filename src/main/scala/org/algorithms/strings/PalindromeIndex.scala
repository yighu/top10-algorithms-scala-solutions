package org.algorithms.strings

object PalindromeIndex extends App {
  def palindromeIndex(s: String): Int = {
    var pIndex = -1
    val len = s.length
    var found = false
    for ( i <- 0 until len /2 if !found) {
      if (s.charAt(i) != s.charAt(len-i-1)){
        if ( i+1 < len){
          val sub = s.substring(i+1, len -i)
          val isPalindrom = sub == sub.reverse
          if (isPalindrom){
            pIndex = i
          }else {
            pIndex = len-i-1
          }
          found = true
        }
      }
    }
    pIndex
  }



  val input ="aabxccccbaa"
  println(palindromeIndex(input))
}
