package org.algorithms.arrays

object KthSmallestLargestElementinUnsortedArray extends App{
  /**
   * https://www.geeksforgeeks.org/kth-smallest-largest-element-in-unsorted-array/
   * K’th Smallest/Largest Element in Unsorted Array
   * Given an array arr[] of size N and a number K, where K is smaller than the size of the array.
   * Find the K’th smallest element in the given array. Given that all array elements are distinct.

      Examples:

      Input: arr[] = {7, 10, 4, 3, 20, 15}, K = 3
      Output: 7

      Input: arr[] = {7, 10, 4, 3, 20, 15}, K = 4
      Output: 10
   */

  import java.util

  def kthSmallest(arr: Array[Int], K: Int) = {
    // Sort the given array
    util.Arrays.sort(arr)
    // Return K'th element in
    // the sorted array
    arr(K - 1)
  }

  val arr = Array[Int](12, 3, 5, 7, 19)
  val K = 2

  // Function call
  print("K'th smallest element is " + kthSmallest(arr, K))
}
