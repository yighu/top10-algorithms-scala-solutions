package org.algorithms.strings

object KMPAlgorithmforPatternSearching extends App {
  /**
   * https://www.geeksforgeeks.org/kmp-algorithm-for-pattern-searching/
   * KMP Algorithm for Pattern Searching
   * Given a text txt[0 . . . N-1] and a pattern pat[0 . . . M-1],
   * write a function search(char pat[], char txt[]) that prints all occurrences of pat[] in txt[].
   * You may assume that N > M.
      Examples:

      Input:  txt[] = “THIS IS A TEST TEXT”, pat[] = “TEST”
      Output: Pattern found at index 10

      Input:  txt[] =  “AABAACAADAABAABA”
                pat[] =  “AABA”
      Output: Pattern found at index 0, Pattern found at index 9, Pattern found at index 12
   */

  def KMPSearch(pat: String, txt: String): Unit = {//TODO this needs lots of improve to make it looks like scala code
    val M = pat.length
    val N = txt.length
    // create lps[] that will hold the longest
    // prefix suffix values for pattern
    val lps = new Array[Int](M)
    var j = 0 // index for pat[]
    // Preprocess the pattern (calculate lps[]
    // array)
    computeLPSArray(pat, M, lps)
    var i = 0 // index for txt[]
    while ((N - i) >= (M - j)) {
      if (pat.charAt(j) == txt.charAt(i)) {
        j += 1
        i += 1
      }
      if (j == M) {
        System.out.println("Found pattern " + "at index " + (i - j))
        j = lps(j - 1)
      }
      else { // mismatch after j matches
        if (i < N && pat.charAt(j) != txt.charAt(i)) { // Do not match lps[0..lps[j-1]] characters,
          // they will match anyway
          if (j != 0) j = lps(j - 1)
          else i = i + 1
        }
      }
    }
  }

  def computeLPSArray(pat: String, M: Int, lps: Array[Int]): Unit = {
    // length of the previous longest prefix suffix
    var len: Int = 0
    var i: Int = 1
    lps(0) = 0
    // lps[0] is always 0
    // the loop calculates lps[i] for i = 1 to M-1
    while (i < M) {
      if (pat.charAt(i) == pat.charAt(len)) {
        len += 1
        lps(i) = len
        i += 1
      }
      else {
        // (pat[i] != pat[len])
        // This is tricky. Consider the example.
        // AAACAAAA and i = 7. The idea is similar
        // to search step.
        if (len != 0) {
          len = lps(len - 1)
          // Also, note that we do not increment
          // i here
        }
        else {
          // if (len == 0)
          lps(i) = len
          i += 1
        }
      }
    }
  }

  val txt = "ABABDABACDABABCABAB"
  val pat = "ABABCABAB"
  KMPSearch(pat, txt)
}
