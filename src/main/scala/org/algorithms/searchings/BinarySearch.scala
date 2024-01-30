package org.algorithms.searchings

import scala.annotation.tailrec

object BinarySearch extends App {
  /**
   * https://www.geeksforgeeks.org/binary-search/
   * Binary Search
   * Divide the search space into two halves by finding the middle index “mid”.
    Compare the middle element of the search space with the key.
    If the key is found at middle element, the process is terminated.
    If the key is not found at middle element, choose which half will be used as the next search space.
        If the key is smaller than the middle element, then the left side is used for next search.
        If the key is larger than the middle element, then the right side is used for next search.
    This process is continued until the key is found or the total search space is exhausted.
   */

  // Returns index of x if it is present in arr[l..// Returns index of x if it is present in arr[l..

  // r], else return -1
  def binarySearch(arr: Array[Int], l: Int, r: Int, x: Int): Int = {
    if (r >= l) {
      val mid = l + (r - l) / 2
      // If the element is present at the
      // middle itself
      if (arr(mid) == x) mid
      // If element is smaller than mid, then
      // it can only be present in left subarray
      else if (arr(mid) > x) binarySearch(arr, l, mid - 1, x)
      // Else the element can only be present
      // in right subarray
      else binarySearch(arr, mid + 1, r, x)
    }else -1
    // We reach here when element is not present
    // in array

  }

  val arr = Array(2, 3, 4, 10, 40)
  val n = arr.length
  val x = 10
  val result = binarySearch(arr, 0, n - 1, x)
  println(result)
}
