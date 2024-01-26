package org.algorithms.arrays

import scala.math.max

object LargestSumContiguousSubarray extends App {

  /**
   * Largest Sum Contiguous Subarray (Kadane’s Algorithm)
   * https://www.geeksforgeeks.org/largest-sum-contiguous-subarray/
   *
   * Given an array arr[] of size N. The task is to find the sum of the
   * contiguous subarray within a arr[] with the largest sum.
   *
   * Pseudocode of Kadane’s algorithm:
   * Initialize:
   * max_so_far = INT_MIN
   * max_ending_here = 0
   *
   * Loop for each element of the array
   *
   * (a) max_ending_here = max_ending_here + a[i]
   * (b) if(max_so_far < max_ending_here)
   * max_so_far = max_ending_here
   * (c) if(max_ending_here < 0)
   * max_ending_here = 0
   * return max_so_far
   */


  def maxSubArraySum(a: Array[Int]): Int = {
    var max_so_far = 0
    var max_ending_here = 0
    for (d <- a) {
      max_ending_here = max_ending_here + d
      max_so_far = max(max_ending_here, max_so_far)
      max_ending_here = max(0, max_ending_here)
    }
    max_so_far
  }

  val maxSum = maxSubArraySum(Array(-2, -3, 4, -1, -2, 1, 5, -3))
  System.out.println("Maximum contiguous sum is " + maxSum)
}
