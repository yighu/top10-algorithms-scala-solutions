package org.algorithms.bitmanipulations

object MaximumsubarrayXORinArray extends App {
  /**
   * https://www.geeksforgeeks.org/find-the-maximum-subarray-xor-in-a-given-array/
   * Find the maximum subarray XOR in a given array
   *
   */

  def maxSubarrayXOR(arr: Array[Int], n: Int): Int = {
    var ans = Integer.MIN_VALUE
    for (i <- 0 until n) {
      var curr_xor = 0
      for (j <- i until n) {
        curr_xor = curr_xor ^ arr(j)
        ans = Math.max(ans, curr_xor)
      }
    }
    ans
  }

  val arr = Array(8, 1, 2, 12)
  val n = arr.length
  println("Max subarray XOR is " + maxSubarrayXOR(arr, n))
}
