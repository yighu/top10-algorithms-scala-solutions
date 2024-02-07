package org.algorithms.dynamicprogrammings

object LongestPalindromicSubsequenceLPS extends App {
  /**
   * https://www.geeksforgeeks.org/longest-palindromic-subsequence-dp-12/
   * Longest Palindromic Subsequence (LPS)
   *
   * Given a string ‘S’, find the length of the Longest Palindromic Subsequence in it.

     The Longest Palindromic Subsequence (LPS) is the problem of finding a maximum-length subsequence of a given
     string that is also a Palindrome.
     Using the Tabulation technique of Dynamic programming to find LPS: the Bottom-up dynamic programming method

   *
   */

  def longestPalinSubseq(S: String): Int = {
    val R = S.reverse
    // dp[i][j] will store the length of the longest
    // palindromic subsequence for the substring
    // starting at index i and ending at index j
    val dp = Array.ofDim[Int](S.length + 1, R.length + 1)
    // Filling up DP table based on conditions discussed
    // in above approach
    for (i <- 1 to S.length) {
      for (j <- 1 to R.length) {
        if (S.charAt(i - 1) == R.charAt(j - 1)) dp(i)(j) = 1 + dp(i - 1)(j - 1)
        else dp(i)(j) = Math.max(dp(i)(j - 1), dp(i - 1)(j))
      }
    }
    // At the end DP table will contain the LPS
    // So just return the length of LPS
    dp(S.length)(R.length)
  }

  val s = "GEEKSFORGEEKS"
  println("The length of the LPS is " + longestPalinSubseq(s))
}
