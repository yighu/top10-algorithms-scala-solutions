package org.algorithms.strings

import scala.annotation.tailrec

object MinimumCharactersTobeAddedatFronttoMakeStringPalindrome extends App {
  /**
   * https://www.geeksforgeeks.org/minimum-characters-added-front-make-string-palindrome/
   * Minimum characters to be added at front to make string palindrome
   * Given string str we need to tell minimum characters to be added in front of the string to make string palindrome.

      Examples:

      Input  : str = "ABC"
      Output : 2
      We can make above string palindrome as "CBABC"
      by adding 'B' and 'C' at front.
      Input  : str = "AACECAAAA";
      Output : 2
      We can make above string palindrome as AAAACECAAAA
      by adding two A's at front of string.

     Another approach using “Two Pointers”:-

      Initialize two pointers start and end to the beginning and end of the string, respectively.
      While start is less than end, if the characters at the start and end pointers are equal,
      move the start pointer one position to the right and the end pointer one position to the left.
      If the characters are not equal, increment the res variable (which keeps track of the number of
      characters that need to be added) and reset the start and end pointers to the beginning and end of the
      string with a reduced number of characters.
      When start is no longer less than end, return the value of res as the minimum number of characters that
      need to be added to the front of the string to make it a palindrome.
   */

  def addMinChar(str1: String) = {
    val n = str1.length
    @tailrec
    def basics(start: Int, end: Int, res: Int): Int = {
      if (start >= end) res // Return the count of characters to be added
      else{
          if (str1.charAt(start) == str1.charAt(end)) {
            // If the characters at the start and end pointers are equal
             // Move the start pointer to the right
             // Move the end pointer to the left
            basics(start + 1, end - 1, res)
          }else {
            // Increment the count of characters to be added
            // Reset the start pointer to the beginning of the string
            // Reset the end pointer to the end of the string with a reduced number of characters
            basics(0, n - res - 1, res + 1)
          }
         }

    }

    basics(0, n-1, 0)

  }

  val str = "AACECAAAA"
  println(addMinChar(str))
}
