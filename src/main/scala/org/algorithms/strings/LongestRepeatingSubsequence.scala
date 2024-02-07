package org.algorithms.strings

object LongestRepeatingSubsequence extends App{
  /**
   * https://www.geeksforgeeks.org/longest-repeating-subsequence/
   * Longest Repeating Subsequence
     Given a string, find the length of the longest repeating subsequence, such that the two subsequences
     don’t have same string character at the same position, i.e. any ith character in the two subsequences
     shouldn’t have the same index in the original string.
      To find the length of the Longest Repeating Subsequence dynamic  programming Top-down Approach:

      Take the input string.
      Perform the Longest common subsequence where s1[i]==s1[j] and i!=j.
      Return the length.
   */
  def findLongestRepeatingSubSeq(str: String): Int = {
    val n = str.length
    // Create and initialize DP table
    val dp :Array[Array[Int]] = Array.ofDim[Int](n+1, n+1)
    // Fill dp table (similar to LCS loops)
    for (i <- 1 to n) {
      for (j <- 1 to n) {
        dp(i)(j) = if (str.charAt(i - 1) == str.charAt(j - 1) && i != j) 1 + dp(i - 1)(j - 1) //characters match and indexes are not same
                   else Math.max(dp(i)(j - 1), dp(i - 1)(j)) //If characters do not match
      }
    }
    dp(n)(n)
  }

  val str = "aabb"
  println("The length of the largest subsequence that " +
    "repeats itself is : " + findLongestRepeatingSubSeq(str))
}
