package org.algorithms.sortings

import scala.annotation.tailrec

object InsertionSort extends App {
  /**
   * https://www.geeksforgeeks.org/insertion-sort/
   * Insertion Sort
   * To sort an array of size N in ascending order iterate over the array and compare the current element (key) to its
   * predecessor, if the key element is smaller than its predecessor, compare it to the elements before. Move the
   * greater elements one position up to make space for the swapped element.
   */

  def sort(arr: Array[Int]): Unit = {
    val n = arr.length
    @tailrec
    def movePreviousUntilGoodForKey(key: Int, j: Int):Int = {
     if (j >= 0 && arr(j) > key) {
       arr(j + 1) = arr(j)
       movePreviousUntilGoodForKey(key, j - 1)
     }
     else j
    }
    @tailrec
    def putEachToRightSpot(i: Int): Unit={
      if (i < n) {
        val key = arr(i)
        val j = movePreviousUntilGoodForKey(key, i - 1)
        arr(j + 1) = key
        putEachToRightSpot(i+1)
      }
    }
    putEachToRightSpot(1)
  }

  val arr = Array(12, 11, 13, 5, 6)
  sort(arr)
  arr.foreach(println)
}
