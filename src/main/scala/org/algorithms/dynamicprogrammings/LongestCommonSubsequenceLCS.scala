package org.algorithms.dynamicprogrammings

object LongestCommonSubsequenceLCS extends App {
  /**
   * https://www.geeksforgeeks.org/longest-common-subsequence-dp-4/
   * Longest Common Subsequence (LCS)
   * Given two strings, S1 and S2, the task is to find the length of the Longest Common Subsequence, i.e.
   * longest subsequence present in both of the strings.
   *
   */
  def lcs(X: String, Y: String, m: Int, n: Int): Int = {
    if (m == 0 || n == 0) 0
    else if (X.charAt(m - 1) == Y.charAt(n - 1)) 1 + lcs(X, Y, m - 1, n - 1)
    else Math.max(lcs(X, Y, m, n - 1), lcs(X, Y, m - 1, n))
  }

  val S1 = "AGGTAB"
  val S2 = "GXTXAYB"
  val m = S1.length
  val n = S2.length

  System.out.println("Length of LCS is" + " " + lcs(S1, S2, m, n))
}
