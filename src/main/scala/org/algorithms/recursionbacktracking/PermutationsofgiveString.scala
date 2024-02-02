package org.algorithms.recursionbacktracking

object PermutationsofgiveString extends App {
  /**
   * https://www.geeksforgeeks.org/write-a-c-program-to-print-all-permutations-of-a-given-string/
   * Permutations of given String
   * Given a string S, the task is to write a program to print all permutations of a given string.

     A permutation also called an “arrangement number” or “order,” is a rearrangement of the elements of an ordered list S
     into a one-to-one correspondence with S itself. A string of length N has N! permutations.
   */

  /**
   * permutation function
   *
   * @param str string to calculate permutation for
   * @param l   starting index
   * @param r   end index
   */

  def permute(str: String, l: Int, r: Int): Unit = {
    if (l == r) println(str)
    else  ( l to r).scanLeft(str){(_, i) =>
      val swapped = swap(str, l, i)
      permute(swapped, l + 1, r)
      swap(swapped, l, i)
    }
  }
  /**
   * Swap Characters at position
   *
   * @param a string value
   * @param i position 1
   * @param j position 2
   * @return swapped string
   */
  def swap(a: String, i: Int, j: Int) = {
    val charArray = a.toCharArray
    val(ci,cj) = (charArray(i), charArray(j))
    charArray(i) = cj
    charArray(j) = ci
    String.valueOf(charArray)
  }
  val str = "ABC";
  val n = str.length();
  permute(str, 0, n - 1)
  println("Scala built in solution")
  str.permutations.foreach(println)
}
