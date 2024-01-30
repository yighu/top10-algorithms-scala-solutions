package org.algorithms.sortings
object SelectionSort extends App {
  /**
   * https://www.geeksforgeeks.org/selection-sort/
   * Selection Sort
   * Selection sort is a simple and efficient sorting algorithm that works by repeatedly selecting the smallest
   * (or largest) element from the unsorted portion of the list and moving it to the sorted portion of the list.
   */

  def sort(arr: Array[Int]): Unit = {
    val n = arr.length
    def swap(i:Int,j: Int)={
      val (a,b) = (arr(i),arr(j))
      arr(i) = b
      arr(j) = a
    }
    // One by one move boundary of unsorted subarray
    for (i <- 0 until n - 1) { // Find the minimum element in unsorted array
      val min_idx = (i+1 until  n).fold(i) {(j, k) =>if (arr(j) < arr(k)) j else k }
      swap(i, min_idx)
    }
  }

  val arr = Array(64, 25, 12, 22, 11)
  sort(arr)
  arr.foreach(println)
}
