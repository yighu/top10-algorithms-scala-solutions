package org.algorithms.recursionbacktracking

object WordBreakProblemusingBacktracking extends App {
  /**
   * https://www.geeksforgeeks.org/word-break-problem-using-backtracking/
   * Word Break Problem using Backtracking
   *
   * Given a valid sentence without any spaces between the words and a dictionary of valid English words, find all
   * possible ways to break the sentence into individual dictionary words.
   */

  import java.util

  def wordBreak(n: Int, dict: util.List[String], s: String): Unit = {
    wordBreakUtil(n, s, dict, "")
  }

  def wordBreakUtil(n: Int, s: String, dict: util.List[String], ans: String): Unit = {
    (1 to n).scanLeft(""){(_, i)=>
      val prefix = s.substring(0, i)
      if (dict.contains(prefix)) {
        if (i == n) {
          println(ans + prefix)
        }
        wordBreakUtil(n - i, s.substring(i, n), dict, ans + prefix + " ")
      }
      ans
    }
  }
  // main function
    val str1 = "iloveicecreamandmango" // for first test case
    val str2 = "ilovesamsungmobile" // for second test case
    val n1 = str1.length // length of first string
    val n2 = str2.length // length of second string
    // List of strings in dictionary
    val dict = util.Arrays.asList("mobile", "samsung", "sam", "sung", "man", "mango", "icecream", "and", "go", "i", "love", "ice", "cream")
    println("First Test:")
    // call to the method
    wordBreak(n1, dict, str1)
    println("\nSecond Test:")
    wordBreak(n2, dict, str2)

}
