package org.algorithms.strings

object CheckifagivenStringisaRotationofaPalindrome extends App {
  /**
   * https://www.geeksforgeeks.org/check-given-string-rotation-palindrome/
   * Check if a given string is a rotation of a palindrome
   * Given a string, check if it is a rotation of a palindrome.
   * For example your function should return true for “aab” as it is a rotation of “aba”.


   */
  def isRotationOfPalindrome(str: String): Boolean = {
    def isPalindrome(s: String) :Boolean = s == s.reverse
    def rotate(s: String): String = s.last + s.dropRight(1)
    def loop(s: String, r: Int): Boolean = {
      val isP = isPalindrome(s)
      if (isP || r == 0) isP
      else loop(rotate(s), r - 1)
    }
    loop(str, str.length)
  }

  System.out.println(if (isRotationOfPalindrome("aab")) 1 else 0)
  System.out.println(if (isRotationOfPalindrome("abcde")) 1 else 0)
  System.out.println(if (isRotationOfPalindrome("aaaad")) 1 else 0)
}
