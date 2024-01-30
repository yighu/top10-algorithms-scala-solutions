package org.algorithms.sortings

import scala.annotation.tailrec

object HeapSort extends App {
  /**
   * https://www.geeksforgeeks.org/heap-sort/
   * Heap Sort
   * To solve the problem follow the below idea:

    First convert the array into heap data structure using heapify, then one by one delete the root node of
    the Max-heap and replace it with the last node in the heap and then heapify the root of the heap.
    Repeat this process until size of heap is greater than 1.

    Build a heap from the given input array.
    Repeat the following steps until the heap contains only one element:
      Swap the root element of the heap (which is the largest element) with the last element of the heap.
      Remove the last element of the heap (which is now in the correct position).
      Heapify the remaining elements of the heap.
    The sorted array is obtained by reversing the order of the elements in the input array.
   */
def sort(arr:Array[Int]): Unit = {
  val N = arr.length
  def heapSort(): Unit = {

    // Build heap (rearrange array)
    for (i <- N / 2 - 1 to 0 by -1) {
      heapify(N, i)
    }
    // One by one extract an element from heap
    for (i <- N - 1 until 0 by -1) { // Move current root to end
      val temp = arr(0)
      arr(0) = arr(i)
      arr(i) = temp
      // call max heapify on the reduced heap
      heapify(i, 0)
    }
  }
  def swap(i:Int, j: Int) = {
    val (a,b) = (arr(i), arr(j))
    arr(i) = b
    arr(j) = a
  }
  // To heapify a subtree rooted with node i which is
  // an index in arr[]. heapSize is size of heap
  @tailrec
  def heapify(heapSize: Int, i: Int): Unit = {
    var largest: Int = i // Initialize largest as root
    val l: Int = 2 * i + 1 // left = 2*i + 1
    val r: Int = 2 * i + 2 // right = 2*i + 2
    // If left child is larger than root
    if (l < heapSize && arr(l) > arr(largest)) {
      largest = l
    }
    // If right child is larger than largest so far
    if (r < heapSize && arr(r) > arr(largest)) {
      largest = r
    }
    // If largest is not root
    if (largest != i) {
      swap(i, largest)
      // Recursively heapify the affected sub-tree
      heapify(heapSize, largest)
    }
  }
  heapSort()
}

  val arr = Array(12, 11, 13, 5, 6, 7)
  sort(arr)
  arr.foreach(println)
}
