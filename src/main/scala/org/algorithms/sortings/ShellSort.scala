package org.algorithms.sortings

import scala.annotation.tailrec

object ShellSort extends App {
  /**
   * https://www.geeksforgeeks.org/shellsort/
   * ShellSort
   * Shell sort is mainly a variation of Insertion Sort. In insertion sort, we move elements only one position ahead.
   * When an element has to be moved far ahead, many movements are involved. The idea of ShellSort is to allow the
   * exchange of far items. In Shell sort, we make the array h-sorted for a large value of h. We keep reducing the
   * value of h until it becomes 1. An array is said to be h-sorted if all sublists of every h’th element are sorted.

      Algorithm:

      Step 1 − Start
      Step 2 − Initialize the value of gap size. Example: h
      Step 3 − Divide the list into smaller sub-part. Each must have equal intervals to h
      Step 4 − Sort these sub-lists using insertion sort
      Step 5 – Repeat this step 2 until the list is sorted.
      Step 6 – Print a sorted list.
      Step 7 – Stop.

   */

  def sort(arr: Array[Int]): Int = {
    val n = arr.length
    @tailrec
    def moveDataForCurrent(gap: Int, j: Int, current: Int):Int = {
      if (j >= gap && arr(j - gap) > current) {
        arr(j) = arr(j - gap)
        moveDataForCurrent(gap, j-gap, current)
      }
      else j
    }

    @tailrec
    def moveWithinGap(i: Int, gap: Int):Unit = {
      if (i<n){
        // add a[i] to the elements that have been gap
        // sorted save a[i] in temp and make a hole at
        // position i
        val temp = arr(i)
        // shift earlier gap-sorted elements up until
        // the correct location for a[i] is found
        val j = moveDataForCurrent(gap, i, temp)
        // put temp (the original a[i]) in its correct
        // location
        arr(j) = temp
        moveWithinGap(i+1, gap)
      }
    }
    @tailrec
    def reduceGap(gap: Int): Unit = {
      if (gap > 0){
        // Do a gapped insertion sort for this gap size.
        // The first gap elements a[0..gap-1] are already
        // in gapped order keep adding one more element
        // until the entire array is gap sorted
        moveWithinGap(gap, gap)
        reduceGap(gap/2)
      }
    }
    // Start with a big gap, then reduce the gap
    reduceGap(n/2)
    0
  }

  val arr = Array(12, 34, 54, 2, 3)
  sort(arr)
  arr.foreach(println)
}
