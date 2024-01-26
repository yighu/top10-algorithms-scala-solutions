package org.algorithms.strings

import scala.math.Ordering.Implicits.infixOrderingOps

object EditDistance extends App{
  /**
   * https://www.geeksforgeeks.org/edit-distance-dp-5/
   * Edit Distance
   * Given two strings str1 and str2 of length M and N respectively and below operations that can be
   * performed on str1. Find the minimum number of edits (operations) to convert ‘str1‘ into ‘str2‘.

    Operation 1 (INSERT): Insert any character before or after any index of str1
    Operation 2 (REMOVE): Remove a character of str1
    Operation 3 (Replace): Replace a character at any index of str1 with some other character.
    Note: All of the above operations are of equal cost.

    Examples:

    Input:   str1 = “geek”, str2 = “gesek”
    Output:  1
    Explanation: We can convert str1 into str2 by inserting a ‘s’ between two consecutive ‘e’ in str2.

   Subproblems in Edit Distance:
    The idea is to process all characters one by one starting from either from left or right sides of both strings.
    Let us process from the right end of the strings, there are two possibilities for every pair of characters
    being traversed, either they match or they don’t match. If last characters of both string matches then
    there is no need to perform any operation So, recursively calculate the answer for rest of part of the strings.
    When last characters do not match, we can perform all three operations to match the last characters in the
    given strings, i.e. insert, replace, and remove. We then recursively calculate the result for the remaining
    part of the string. Upon completion of these operations, we will select the minimum answer.

   When the last characters of strings matches. Make a recursive call EditDistance(M-1,N-1) to calculate
   the answer for remaining part of the strings.

    When the last characters of strings don’t matches. Make three recursive calls as show below:

    Insert str1[N-1] at last of str2 : EditDistance(M, N-1)
    Replace str2[M-1] with str1[N-1] : EditDistance(M-1, N-1)
    Remove str2[M-1] : EditDistance(M-1, N)
   Base Case for Edit Distance:
    Case 1: When str1 becomes empty i.e. M=0
      return N, as it require N characters to convert an empty string to str1 of size N
    Case 2: When str2 becomes empty i.e. N=0
      return M, as it require M characters to convert an empty string to str2 of size M
   */

  def editDist(str1: String, str2: String, m: Int, n: Int): Int = {
    // If first string is empty, the only option is to
    // insert all characters of second string into first
    if (m == 0) n
    // If second string is empty, the only option is to
    // remove all characters of first string
    else if (n == 0) m
    // If last characters of two strings are same,
    // nothing much to do. Get the count for remaining strings.
    else if (str1.charAt(m - 1) == str2.charAt(n - 1)) editDist(str1, str2, m - 1, n - 1)
    // If last characters are not same, consider all
    // three operations on last character of first
    // string, recursively compute minimum cost for all
    // three operations and take minimum of three
    // values.
    else
      1 + List(editDist(str1, str2, m, n - 1), // Insert
               editDist(str1, str2, m - 1, n), // Remove
               editDist(str1, str2, m - 1, n - 1)).min // Replace
  }

  val str1 = "sunday"
  val str2 = "saturday"

  System.out.println(editDist(str1, str2, str1.length, str2.length))
}
