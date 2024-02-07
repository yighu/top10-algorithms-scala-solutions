package org.algorithms.bitmanipulations

object SumofXORofallsubarrays extends App {
  /**
   * https://www.geeksforgeeks.org/sum-of-xor-of-all-subarrays/
   * Sum of XOR of all subarrays
     Given an array containing N positive integers, the task is to find the sum of XOR of all sub-arrays of the array.
   */
  def findXorSum(arr: Array[Int], n: Int): Int = {
    var sum = 0
    for (i <- 0 until n) {
      var xorr = 0
      for (j <- i until n) {
        xorr = xorr ^ arr(j)
        sum += xorr
      }
    }
    sum
  }

  val arr = Array(3, 8, 13)

  val n = arr.length

  println(findXorSum(arr, n))
}
