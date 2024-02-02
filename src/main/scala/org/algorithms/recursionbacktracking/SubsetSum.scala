package org.algorithms.recursionbacktracking

object SubsetSum extends App {
  /**
   * https://www.geeksforgeeks.org/subset-sum-problem-dp-25/
   * Subset Sum
   * Given a set of non-negative integers and a value sum, the task is to check if there is a subset of the given
   * set whose sum is equal to the given sum.
   * isSubsetSum(set, n, sum) = isSubsetSum(set, n-1, sum) | isSubsetSum(set, n-1, sum-set[n-1])

      Base Cases:
      isSubsetSum(set, n, sum) = false, if sum > 0 and n = 0
      isSubsetSum(set, n, sum) = true, if sum = 0

   Follow the below steps to implement the recursion:

    Build a recursive function and pass the index to be considered (here gradually moving from the last end) and the
      remaining sum amount.
    For each index check the base cases and utilize the above recursive call.
    If the answer is true for any recursion call, then there exists such a subset. Otherwise, no such subset exists.
    Below is the implementation of the above approach.
   */

  def isSubsetSum(data: Array[Int], len: Int, remains: Int): Boolean = { // Base Cases
    if (remains == 0) true
    else if (len == 0) false
    // If last element is greater than
    // sum, then ignore it
    else if (data(len - 1) > remains) isSubsetSum(data, len - 1, remains)
    // Else, check if sum can be obtained
    // by any of the following
    // (a) including the last element
    // (b) excluding the last element
    else isSubsetSum(data, len - 1, remains) || isSubsetSum(data, len - 1, remains - data(len - 1))
  }

  val set = Array(3, 34, 4, 12, 5, 2)
  val sum = 11
  val n = set.length
  if (isSubsetSum(set, n, sum)) println("Found a subset" + " with given sum")
  else println("No subset with" + " given sum")
}
