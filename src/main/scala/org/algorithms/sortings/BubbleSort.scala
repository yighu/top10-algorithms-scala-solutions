package org.algorithms.sortings

object BubbleSort extends App {
  /**
   * https://www.geeksforgeeks.org/bubble-sort/
   * Bubble Sort
   * traverse from left and compare adjacent elements and the higher one is placed at right side.
     In this way, the largest element is moved to the rightmost end at first.
     This process is then continued to find the second largest and place it and so on until the data is sorted.

   */

  // An optimized version of Bubble Sort// An optimized version of Bubble Sort

  def bubbleSort(arr: Array[Int], n: Int): Unit = {
    def swapNext(j: Int) = {
      val(n,m) = (arr(j+1), arr(j))
      arr(j) = n
      arr(j + 1) = m
    }
    def doBubble(j:Int, i: Int, swapped: Boolean): Boolean = {
      if (j < n -i -1) {
        if (arr(j) > arr(j + 1)) {
          swapNext(j)  // Swap arr[j] and arr[j+1]
          doBubble(j+1, i, true)
        }else doBubble(j+1, i, swapped)
      }
      else swapped
    }

    def doScanBubble(i: Int, swapped: Boolean): Unit = {
      if ( swapped && i < n -1 ) doScanBubble(i+1, doBubble(0, i, false))
    }
    doScanBubble(0, true)
  }

  val arr: Array[Int] = Array(64, 34,50, 25, 12, 22, 11, 90)
  val n: Int = arr.length
  bubbleSort(arr, n)
  arr.foreach(println)
}
