package org.algorithms.arrays

object MaximumProductSubarray extends App{
  /**
   * https://www.geeksforgeeks.org/maximum-product-subarray/
   * Given an array that contains both positive and negative integers,
   * the task is to find the product of the maximum product subarray.
   * Maximum Product Subarray using Kadane’s Algorithm
   * The idea is to use Kadane’s algorithm and maintain 3 variables max_so_far, max_ending_here & min_ending_here.
   * Iterate the indices 0 to N-1 and update the variables such that:
        max_ending_here = maximum(arr[i], max_ending_here * arr[i], min_ending_here[i]*arr[i])
        min_ending_here = maximum(arr[i], max_ending_here * arr[i], min_ending_here[i]*arr[i])
        update the max_so_far with the maximum value for each index.
     return max_so_far as the result.
   */


  def maxSubarrayProduct(arr: Array[Int], n: Int): Int = { // max positive product
    // ending at the current position
    var max_ending_here = arr(0)
    // min negative product ending
    // at the current position
    var min_ending_here = arr(0)
    // Initialize overall max product
    var max_so_far = arr(0)
    // /* Traverse through the array.
    // the maximum product subarray ending at an index
    // will be the maximum of the element itself,
    // the product of element and max product ending
    // previously and the min product ending previously.
    // */
    for (i <- 1 until n) {
      val temp = Math.max(Math.max(arr(i), arr(i) * max_ending_here), arr(i) * min_ending_here)
      min_ending_here = Math.min(Math.min(arr(i), arr(i) * max_ending_here), arr(i) * min_ending_here)
      max_ending_here = temp
      max_so_far = Math.max(max_so_far, max_ending_here)
    }
    max_so_far
  }

  val arr = Array(1, -2, -3, 0, 7, -8, -2)
  val n = arr.length
  printf("Maximum Sub array product is %d", maxSubarrayProduct(arr, n))
}
