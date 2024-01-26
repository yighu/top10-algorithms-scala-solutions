package org.algorithms.arrays

object FindtheMissingNumber extends App{
  /**
   * https://www.geeksforgeeks.org/find-the-missing-number/
   * Given an array arr[] of size N-1 with integers in the range of [1, N], the task is to find
   * the missing number from the first N integers.
   * Note: There are no duplicates in the list.
   * Calculate the sum of the first N natural numbers as sumtotal= N*(N+1)/2.
   * Traverse the array from start to end.
   * Find the sum of all the array elements.
   * Print the missing number as SumTotal – sum of array
   */
  def getMissingNo(nums: Array[Int], n: Int): Int = n * (n+1)/2 - nums.sum

  val arr = Array(1, 2, 3, 5)
  System.out.println(getMissingNo(arr, 5))
}
