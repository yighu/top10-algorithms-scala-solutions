package org.algorithms.sortings

import org.algorithms.sortings.RadixSort.arr

import scala.annotation.tailrec

object RadixSort extends App {
  /**
   * https://www.geeksforgeeks.org/radix-sort/
   * Radix Sort
   * Radix Sort is a linear sorting algorithm that sorts elements by processing them digit by digit.
   * It is an efficient sorting algorithm for integers or strings with fixed-size keys.
    Radix Sort Algorithm
    The key idea behind Radix Sort is to exploit the concept of place value. It assumes that sorting numbers digit
    by digit will eventually result in a fully sorted list. Radix Sort can be performed using different variations,
    such as Least Significant Digit (LSD) Radix Sort or Most Significant Digit (MSD) Radix Sort.
    ex To perform radix sort on the array [170, 45, 75, 90, 802, 24, 2, 66], we follow these steps:

    Step 1: Find the largest element in the array, which is 802. It has three digits, so we will iterate three times,
            once for each significant place.

    Step 2: Sort the elements based on the unit place digits (X=0). We use a stable sorting technique,
            such as counting sort, to sort the digits at each significant place.

            Sorting based on the unit place:
            Perform counting sort on the array based on the unit place digits.
            The sorted array based on the unit place is [170, 90, 802, 2, 24, 45, 75, 66].
    Step 3: Sort the elements based on the tens place digits.

            Sorting based on the tens place:

            Perform counting sort on the array based on the tens place digits.
            The sorted array based on the tens place is [802, 2, 24, 45, 66, 170, 75, 90].
    Step 4: Sort the elements based on the hundreds place digits.

            Sorting based on the hundreds place:

            Perform counting sort on the array based on the hundreds place digits.
            The sorted array based on the hundreds place is [2, 24, 45, 66, 75, 90, 170, 802].
    Step 5: The array is now sorted in ascending order.
            The final sorted array using radix sort is [2, 24, 45, 66, 75, 90, 170, 802].

   */

  def sort(arr: Array[Int]): Unit = {
    val n: Int = arr.length

    // A function to do counting sort of arr[] according to
    // the digit represented by exp.
    def countSort(arr: Array[Int], n: Int, exp: Int): Unit = {
      val output = new Array[Int](n) // output array
      val count = Array.fill(10)(0)
      // Store count of occurrences in count[]
      for {d <- 0 until n} count((arr(d) / exp) % 10) += 1
      // Change count[i] so that count[i] now contains
      // actual position of this digit in output[]
      for { d <- 1 until 10} count(d) += count(d - 1)

      // Build the output array
      for { d <- n-1 to 0 by -1 }{
        output(count((arr(d) / exp) % 10) - 1) = arr(d)
        count((arr(d) / exp) % 10) -= 1
      }

      // Copy the output array to arr[], so that arr[] now
      // contains sorted numbers according to current
      // digit
      output.copyToArray(arr)

    }

    // The main function to that sorts arr[] of
    // size n using Radix Sort
    def radixsort(arr: Array[Int], n: Int): Unit = { // Find the maximum number to know number of digits
      val m = arr.max
      @tailrec
      def sortDigit(exp: Int): Unit = {
        if (m/exp >0) {
          countSort(arr, n , exp)
          sortDigit(exp * 10)
        }
      }
      // Do counting sort for every digit. Note that
      // instead of passing digit number, exp is passed.
      // exp is 10^i where i is current digit number
      sortDigit(1)

    }
    radixsort(arr,n)
  }
  val arr = Array(170, 45, 75, 90, 802, 24, 2, 66)
  sort(arr)

  arr.foreach(println)

}
