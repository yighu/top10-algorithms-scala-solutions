package org.algorithms.sortings

import org.algorithms.sortings.QuickSort.arr

import scala.annotation.tailrec

object QuickSort extends App {
  /**
   * https://www.geeksforgeeks.org/quick-sort/
   * QuickSort
   * QuickSort is a sorting algorithm based on the Divide and Conquer algorithm that picks an element as a pivot
   * and partitions the given array around the picked pivot by placing the pivot in its correct position in the
   * sorted array.
   *
   * How does QuickSort work?
     The key process in quickSort is a partition(). The target of partitions is to place the pivot
     (any element can be chosen to be a pivot) at its correct position in the sorted array and put all smaller
     elements to the left of the pivot, and all greater elements to the right of the pivot.

     Partition is done recursively on each side of the pivot after the pivot is placed in its correct position
     and this finally sorts the array.

     Partition Algorithm:
     The logic is simple, we start from the leftmost element and keep track of the index of smaller (or equal)
     elements as i. While traversing, if we find a smaller element, we swap the current element with arr[i].
     Otherwise, we ignore the current element.

     As the partition process is done recursively, it keeps on putting the pivot in its actual position in the
     sorted array. Repeatedly putting pivots in their actual position makes the array sorted.

   */
  def sort(arr: Array[Int]) = {
    // A utility function to swap two elements
    def swap(i: Int, j: Int): Unit = {
      val(ai, aj) = (arr(j), arr(i))
      arr(i) = ai
      arr(j) = aj
    }

    // This function takes last element as pivot,
    // places the pivot element at its correct position
    // in sorted array, and places all smaller to left
    // of pivot and all greater elements to right of pivot
    def partition(low: Int, high: Int): Int = { // Choosing the pivot
      val pivot: Int = arr(high)
      // Index of smaller element and indicates
      // the right position of pivot found so far
      var i: Int = (low - 1)
      for (j <- low to high - 1) { // If current element is smaller than the pivot
        if (arr(j) < pivot) { // Increment index of smaller element
          i += 1
          swap(i, j)
        }
      }
      swap(i + 1, high)
      i + 1
    }

    // The main function that implements QuickSort
    // arr[] --> Array to be sorted,
    // low --> Starting index,
    // high --> Ending index
    def quickSort(low: Int, high: Int): Unit = {
      if (low < high) {
        // pi is partitioning index, arr[p]
        // is now at right place
        val pi: Int = partition(low, high)
        // Separately sort elements before
        // partition and after partition
        quickSort(low, pi - 1)
        quickSort(pi + 1, high)
      }
    }
    val N = arr.length
    quickSort(0, N-1)
  }

  //val N = arr.length

  // Function call
  //quickSort(0, N - 1)
  val arr = Array(10, 7, 8, 9, 1, 5)

  sort(arr)
  System.out.println("Sorted array:")
  arr.foreach(println)
}
