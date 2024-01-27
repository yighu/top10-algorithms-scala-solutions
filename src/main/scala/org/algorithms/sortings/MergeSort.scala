package org.algorithms.sortings

import org.algorithms.sortings.MergeSort.arr

object MergeSort extends App {
  /**
   * https://www.geeksforgeeks.org/merge-sort/
   * Merge Sort
   * Merge sort is defined as a sorting algorithm that works by dividing an array into smaller subarrays,
   * sorting each subarray, and then merging the sorted subarrays back together to form the final sorted array.
   *
   * How does Merge Sort work?
     Merge sort is a recursive algorithm that continuously splits the array in half until it cannot be further
     divided i.e., the array has only one element left (an array with one element is always sorted).
     Then the sorted subarrays are merged into one sorted array.
   */

  // Merges two subarrays of arr[].// Merges two subarrays of arr[].
def sort(arr: Array[Int]) = {

    def stepMerge(i: Int, j: Int, k:Int, nl:Int, nr: Int, L: Array[Int], R: Array[Int]): Unit={
      if (i < nl && j < nr) {
        if (L(i) <= R(j)) {
          arr(k) = L(i)
          stepMerge(i+1, j, k+1, nl, nr, L, R)
        }
        else {
          arr(k) = R(j)
          stepMerge(i, j+1, k+1, nl, nr, L, R)
        }
      }
      else if (i < nl) {
        arr(k) = L(i)
        stepMerge(i+1, j, k+1, nl, nr, L, R)
      } else if (j < nr) {
        arr(k) = R(j)
        stepMerge(i, j+1, k+1, nl, nr, L, R)
      }

    }
    // First subarray is arr[l..m]
    // Second subarray is arr[m+1..r]
    def merge(l: Int, m: Int, r: Int): Unit = { // Find sizes of two subarrays to be merged
      stepMerge(0, 0, l, m - l + 1, r - m, arr.slice(l, m  + 1), arr.slice(m + 1,  1 + r ))
    }

    // Main function that sorts arr[l..r] using
    // merge()
    def mergeSort(l: Int, r: Int): Unit = {
      if (l < r) { // Find the middle point
        val m: Int = l + (r - l) / 2
        // Sort first and second halves
        mergeSort( l, m)
        mergeSort( m + 1, r)
        // Merge the sorted halves
        merge(l, m, r)
      }
    }
    mergeSort( 0, arr.length - 1)
  }

  val arr = Array(12, 11, 13, 5, 6, 7)
  sort(arr)
  arr.foreach(println)
}
