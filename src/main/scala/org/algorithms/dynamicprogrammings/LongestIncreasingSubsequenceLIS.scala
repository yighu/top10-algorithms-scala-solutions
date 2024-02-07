package org.algorithms.dynamicprogrammings

object LongestIncreasingSubsequenceLIS extends App {
  /**
   * https://www.geeksforgeeks.org/longest-increasing-subsequence-dp-3/
   * Longest Increasing Subsequence (LIS)
   * Given an array arr[] of size N, the task is to find the length of the Longest Increasing Subsequence (LIS) i.e.,
   * the longest possible subsequence in which the elements of the subsequence are sorted in increasing order.
   * Longest Increasing Subsequence using Dynamic Programming:
      Due to optimal substructure and overlapping subproblem property, we can also utilise Dynamic programming to
      solve the problem. Instead of memoization, we can use the nested loop to implement the recursive relation.

      The outer loop will run from i = 1 to N and the inner loop will run from j = 0 to i and use the recurrence
      relation to solve the problem.
   */

  def lis(arr: Array[Int], n: Int): Int = {
    val lisTrack = Array.fill(n)(1)
    for (i <- 1 until  n) {
      for (j <- 0 until  i) {
        if (arr(i) > arr(j) && lisTrack(i) < lisTrack(j) + 1) lisTrack(i) = lisTrack(j) + 1
      }
    }
    lisTrack.max
  }

  val arr = Array(10, 22, 9, 33, 21, 50, 41, 60)
  val n = arr.length
  System.out.println("Length of lis is " + lis(arr, n))
}
