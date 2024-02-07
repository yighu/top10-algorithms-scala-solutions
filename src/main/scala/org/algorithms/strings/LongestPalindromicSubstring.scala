package org.algorithms.strings

object LongestPalindromicSubstring extends App{
  /**
   * https://www.geeksforgeeks.org/longest-palindromic-substring/
   * Longest Palindromic Substring
   * Given a string str, the task is to find the longest substring which is a palindrome.

      Examples:

      Input: str = “forgeeksskeegfor”
      Output: “geeksskeeg”
      Explanation: There are several possible palindromic substrings like “kssk”, “ss”, “eeksskee” etc.
        But the substring “geeksskeeg” is the longest among all.

      Input: str = “Geeks”
      Output: “ee”

      Longest Palindromic Substring using Expansion from center:
      The LPS is either of even length or odd length. So the idea is to traverse the input string and
      for each character check if this character can be the center of a palindromic substring of odd length or
      even length.

      Follow the steps mentioned below to implement the idea:

      Use two pointers, low and hi, for the left and right end of the current palindromic substring being found.
      Then checks if the characters at str[low] and str[hi] are the same.
      If they are, it expands the substring to the left and right by decrementing low and incrementing hi.
      It continues this process until the characters at str[low] and str[hi] are unequal or
      until the indices are in bounds.
      If the length of the current palindromic substring becomes greater than the maximum length,
      it updates the maximum length.

   */

  def longestPalSubstr(s: String): Int = {
    val n = s.length
    var start = 0
    var end = 1
    var low = 0
    var hi = 0
    // Traverse the input string
    for (i <- 0 until n) {
      // Find longest palindromic substring of even size
      low = i - 1
      hi = i
      // Expand substring while it is palindrome and in bounds
      while ( {
        low >= 0 && hi < n && s.charAt(low) == s.charAt(hi)
      }) {
        // Update maximum length and starting index
        if (hi - low + 1 > end) {
          start = low
          end = hi - low + 1
        }
        low -= 1
        hi += 1
      }
      // Find longest palindromic substring of odd size
      low = i - 1
      hi = i + 1
      while ( {
        low >= 0 && hi < n && s.charAt(low) == s.charAt(hi)
      }) {
        if (hi - low + 1 > end) {
          start = low
          end = hi - low + 1
        }
        low -= 1
        hi += 1
      }
    }
    // Print the longest palindromic substring
    System.out.print("Longest palindrome substring is: ")
    //printSubStr(s, start, start + end - 1)
    s.slice(start, start+end).foreach(print)
    println("")
    // Return output length
    return end
  }

  val s = "forgeeksskeegfor"
  val length = longestPalSubstr(s)
  println("Length: " + length)
}
