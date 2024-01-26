package org.algorithms.strings

object WildcardPatternMatching extends App{
  /**
   * https://www.geeksforgeeks.org/wildcard-pattern-matching/
   * Wildcard Pattern Matching
   * Given a text and a wildcard pattern, implement wildcard pattern matching algorithm
   * that finds if wildcard pattern is matched with text. The matching should cover the entire text
   * (not partial text). The wildcard pattern can include the characters ‘?’ and ‘*’

      ‘?’ – matches any single character
      ‘*’ – Matches any sequence of characters (including the empty sequence)
      For example:

      Text = "baaabab",
      Pattern = “*****ba*****ab", output : true
      Pattern = "baaa?ab", output : true
      Pattern = "ba*a?", output : true
      Pattern = "a*ab", output : false

     Let’s consider any character in the pattern.

    Case 1: The character is ‘*’ . Here two cases arises as follows:

    We can ignore ‘*’ character and move to next character in the Pattern.
    ‘*’ character matches with one or more characters in Text. Here we will move to next character in the string.
    Case 2: The character is ‘?’
    We can ignore current character in Text and move to next character in the Pattern and Text.
       Case 3: The character is not a wildcard character
    If current character in Text matches with current character in Pattern, we move to next character in the Pattern
    and Text. If they do not match, wildcard pattern and Text do not match.
    We can use Dynamic Programming to solve this problem:

    Let T[i][j] is true if first i characters in given string matches the first j characters of pattern.

   Approach: Greedy Method

    We know in the greedy algorithm, we always find the temporary best solution and hope that it leads to a
    globally best or optimal solution.

    At first, we initialize two pointers i and j to the beginning of the text and the pattern, respectively.
    We also initialize two variables startIndex and match to -1 and 0, respectively. startIndex will keep
    track of the position of the last ‘*’ character in the pattern, and match will keep track of the
    position in the text where the last proper match started.

    We then loop through the text until we reach the end or find a character in the pattern that doesn’t match
    the corresponding character in the text. If the current characters match, we simply move to the
    next characters in both the pattern and the text. Ifnd if the pattern has a ‘?’ , we simply move to the
    next characters in both the pattern and the text. If the pattern has a ‘ ‘ character, then we mark
    the current position in the pattern and the text as a proper match by setting startIndex to the
    current position in the pattern and its match to the current position in the text.
    If there was no match and no ‘ ‘ character, then we understand we need to go through a different route henceforth,
    we backtrack to the last  ‘*’ character position and try a different match by setting j to startIndex + 1,
    match to match + 1, and i to match.

    Once we have looped over the text, we consume any remaining ‘*’ characters in the pattern, and if we
    have reached the end of both the pattern and the text, the pattern matches the text.
   */


  /**
   * This solution is credit to:
   * https://leetcode.com/problems/wildcard-matching/solutions/3529793/scala-simple-solution/
   * @param s
   * @param p
   * @return
   */
  def isMatch(s: String, p: String): Boolean = {
    val m = s.length
    val n = p.length
    val dp = Array.ofDim[Boolean](m+1, n+1)
    dp(0)(0) = true
    for (j <- 1 to n) if (p(j-1) == '*') dp(0)(j) = dp(0)(j-1)
    for (i <- 1 to m; j <- 1 to n)
      if (p(j-1) == s(i-1) || p(j-1) == '?') dp(i)(j) = dp(i-1)(j-1)
      else if (p(j-1) == '*') dp(i)(j) = dp(i-1)(j) || dp(i)(j-1)

    dp(m)(n)
  }
  val str = "baaabab"
  val pattern = "*****ba*****ab"
  // char pattern[] = "ba*****ab";
  // char pattern[] = "ba*ab";
  // char pattern[] = "a*ab";
  // char pattern[] = "a*****ab";
  // char pattern[] = "*a*****ab";
  // char pattern[] = "ba*ab****";
  // char pattern[] = "****";
  // char pattern[] = "*";
  // char pattern[] = "aa?ab";
  // char pattern[] = "b*b";
  // char pattern[] = "a*a";
  // char pattern[] = "baaabab";
  // char pattern[] = "?baaabab";
  // char pattern[] = "*baaaba*";

  if (isMatch(str, pattern)) System.out.println("Yes")
  else System.out.println("No")
}
