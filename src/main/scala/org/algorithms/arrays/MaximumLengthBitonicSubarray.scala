package org.algorithms.arrays

object MaximumLengthBitonicSubarray extends App {
  /**
   * https://www.geeksforgeeks.org/maximum-length-bitonic-subarray/
   * Maximum Length Bitonic Subarray | Set 1 (O(n) time and O(n) space)
   *
   * Given an array A[0 … n-1] containing n positive integers, a subarray A[i … j] is bitonic
   * if there is a k with i <= k <= j such that A[i] <= A[i + 1] … = A[k + 1] >= .. A[j – 1] > = A[j].
   * Write a function that takes an array as argument and returns the length of the maximum length bitonic subarray.
      Expected time complexity of the solution is O(n)
      Simple Examples
      1) A[] = {12, 4, 78, 90, 45, 23}, the maximum length bitonic subarray is {4, 78, 90, 45, 23} which is of length 5.
      2) A[] = {20, 4, 1, 2, 3, 4, 2, 10}, the maximum length bitonic subarray is {1, 2, 3, 4, 2} which is of length 5.
      Extreme Examples
      1) A[] = {10}, the single element is bitonic, so output is 1.
      2) A[] = {10, 20, 30, 40}, the complete array itself is bitonic, so output is 4.
      3) A[] = {40, 30, 20, 10}, the complete array itself is bitonic, so output is 4.
   */

  def bitonic(arr: Array[Int], n: Int): Int = {
    val inc = new Array[Int](n) // Length of increasing subarray ending
    // at all indexes
    val dec = new Array[Int](n) // Length of decreasing subarray starting

    // Length of increasing sequence ending at first index is 1
    inc(0) = 1
    // Length of increasing sequence starting at first index is 1
    dec(n - 1) = 1
    // Step 1) Construct increasing sequence array
    for (i <- 1 until n) inc(i) = if (arr(i) >= arr(i - 1)) inc(i - 1) + 1 else 1
    // Step 2) Construct decreasing sequence array
    for (i <- n - 2 to 0 by -1) dec(i) = if (arr(i) >= arr(i + 1)) dec(i + 1) + 1 else 1
    // Step 3) Find the length of maximum length bitonic sequence

    ( (inc zip dec) map {case(a,b) => a + b - 1}).max

  }

  val arr = Array(12, 4, 78, 90, 45, 23)
  val n = arr.length
  println("Length of max length Bitonic Subarray is " + bitonic(arr, n))
}
