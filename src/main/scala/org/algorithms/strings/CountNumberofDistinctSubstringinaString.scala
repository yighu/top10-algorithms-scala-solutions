package org.algorithms.strings

import scala.collection.mutable

object CountNumberofDistinctSubstringinaString extends App {
  /**
   * https://www.geeksforgeeks.org/count-number-of-distinct-substring-in-a-string/
   * Count number of Distinct Substring in a String
   *Given a string, count all distinct substrings of the given string.

    Examples:

    Input : abcd
    Output : abcd abc ab a bcd bc b cd c d
    All Elements are Distinct

    Input : aaa
    Output : aaa aa a aa a a
    All elements are not Distinct

   The idea is to use hash table (HashSet in Java) to store all generated substrings.
   Finally we return size of the HashSet.
   */

  def distinctSubstring(str: String): Set[String] = { // Put all distinct substring in a HashSet
    var result: Set[String] = Set()
    // List All Substrings
    for (i <- 0 to str.length) {
      for (j <- i + 1 to str.length) { // Add each substring in Set
        result += str.substring(i, j)
      }
    }
    // Return the HashSet
    result
  }

  val cnt = distinctSubstring("aaa")
  System.out.println("Count of distinct substrings: " + cnt)
}
