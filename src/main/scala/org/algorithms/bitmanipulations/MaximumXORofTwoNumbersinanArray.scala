package org.algorithms.bitmanipulations

object MaximumXORofTwoNumbersinanArray extends App {
  /**
   * https://www.geeksforgeeks.org/maximum-xor-of-two-numbers-in-an-array/
   * Maximum XOR of Two Numbers in an Array
   * Given an array Arr of non-negative integers of size N. The task is to find the maximum possible xor between two
   * numbers present in the array.
   */

  def max_xor(arr:List[Int]): Int ={
    val n = arr.length
    var mx=0
    for {
      i <- 0 until n
      j <- i+1 until n
    } mx = math.max(mx, arr(i) ^ arr(j))
  mx
  }

  val arr = List(25, 10, 2, 8, 5, 3)
  val mxXor = max_xor(arr)
  println(s"max xor $mxXor")
}
