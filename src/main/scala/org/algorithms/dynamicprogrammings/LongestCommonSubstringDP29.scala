package org.algorithms.dynamicprogrammings

object LongestCommonSubstringDP29 extends App {
  /**
   * https://www.geeksforgeeks.org/longest-common-substring-dp-29/
   * Longest Common Substring | DP-29
   * Given two strings ‘X’ and ‘Y’, find the length of the longest common substring.
   * Let m and n be the lengths of the first and second strings respectively.

     Dynamic Programming can be used to find the longest common substring in O(m*n) time. The idea is to find the
     length of the longest common suffix for all substrings of both strings and store these lengths in a table.

     The longest common suffix has following optimal substructure property.

      If last characters match, then we reduce both lengths by 1
        LCSuff(X, Y, m, n) = LCSuff(X, Y, m-1, n-1) + 1 if X[m-1] = Y[n-1]
      If last characters do not match, then result is 0, i.e.,
        LCSuff(X, Y, m, n) = 0 if (X[m-1] != Y[n-1])

      Now we consider suffixes of different substrings ending at different indexes.
      The maximum length Longest Common Suffix is the longest common substring.
      LCSubStr(X, Y, m, n) = Max(LCSuff(X, Y, i, j)) where 1 <= i <= m and 1 <= j <= n
   */

  def LCSubStr(X: Array[Char], Y: Array[Char], m: Int, n: Int): Int = {
    // Create a table to store
    // lengths of longest common
    // suffixes of substrings.
    // Note that LCSuff[i][j]
    // contains length of longest
    // common suffix of
    // X[0..i-1] and Y[0..j-1].
    // The first row and first
    // column entries have no
    // logical meaning, they are
    // used only for simplicity of program
    val LCStuff =  Array.ofDim[Int](m + 1, n + 1)
    // To store length of the longest
    // common substring
    var result = 0
    // Following steps build
    // LCSuff[m+1][n+1] in bottom up fashion
    for (i <- 0 to m) {
      for (j <- 0 to n) {
        if (i == 0 || j == 0) LCStuff(i)(j) = 0
        else if (X(i - 1) == Y(j - 1)) {
          LCStuff(i)(j) = LCStuff(i - 1)(j - 1) + 1
          result = Integer.max(result, LCStuff(i)(j))
        }
        else LCStuff(i)(j) = 0
      }
    }
    result
  }

  val X = "OldSite:GeeksforGeeks.org"
  val Y = "NewSite:GeeksQuiz.com"

  val m = X.length
  val n = Y.length

  System.out.println("Length of Longest Common Substring is " + LCSubStr(X.toCharArray, Y.toCharArray, m, n))
}
