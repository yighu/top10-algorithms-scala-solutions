package org.algorithms.arrays

object SlidingWindow extends App {
  /**
   * Given a array of integers n an a positive number k, find the maximum sum of any contiguous subarray of size k


   */

    def maxSumsubSequenceofSizeK(A: List[Int], k: Int): Int ={
    var maxSum = 0
    for ( i <- 0 to A.length - k){
      maxSum = math.max(maxSum, A.slice(i, i+k).sum)
    }
      maxSum
    }
  //O(N*K)

  //use sliding window

  def maxSumsubSequenceofSizeKSlidingwindow(A: List[Int], k: Int): Int ={
    var maxSum = 0
    var winsum = A.slice(0, k).sum
    for ( i <- k until  A.length ){
       winsum += A(i) - A(i-k)
      maxSum = math.max(maxSum, winsum)
    }
    maxSum
  } //O(N)

  val input = List(2,1,5,1,3,2)
  val k = 3
  val output = 9 //[5,1,3]

  val out = maxSumsubSequenceofSizeK(input, 3)
  println(out)

  val out1 = maxSumsubSequenceofSizeKSlidingwindow(input, 3)
  println(out1)
}
