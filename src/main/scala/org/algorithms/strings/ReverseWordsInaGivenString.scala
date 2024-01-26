package org.algorithms.strings

object ReverseWordsInaGivenString extends App {
  /**
   * https://www.geeksforgeeks.org/reverse-words-in-a-given-string/
   * Reverse words in a given string
   *
   */
  def reverse_words(s: String) : String = {
    def reverse(strs: List[String], acc: String): String = {
      strs match {
        case Nil => acc
        case h::tail => reverse(tail, h+" "+acc)
      }
    }
    reverse(s.split(" ").toList, "")
    }



  var str: String = "i like this program very much"
  str = reverse_words(str)
println(str)
}
