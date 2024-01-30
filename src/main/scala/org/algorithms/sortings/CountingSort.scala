package org.algorithms.sortings

object CountingSort extends App {
  /**
   * https://www.geeksforgeeks.org/counting-sort/
   * Counting Sort
   * Counting Sort is a non-comparison-based sorting algorithm that works well when there is limited range of input
   * values. It is particularly efficient when the range of input values is small compared to the number of elements
   * to be sorted. The basic idea behind Counting Sort is to count the frequency of each distinct element in the
   * input array and use that information to place the elements in their correct sorted positions.
   *  1. Find out the maximum element from the given array.
   *  2. Initialize a countArray[] of length max+1 with all elements as 0. This array will be used for storing
   *     the occurrences of the elements of the input array.
   *  3. In the countArray[], store the count of each unique element of the input array at their respective indices.
   *  4. Store the cumulative sum or prefix sum of the elements of the countArray[] by
   *      doing countArray[i] = countArray[i – 1] + countArray[i]. This will help in placing the elements of the
   *      input array at the correct index in the output array.
   *  5.  Iterate from end of the input array and because traversing input array from end preserves the order of
   *      equal elements, which eventually makes this sorting algorithm stable.
            Update outputArray[ countArray[ inputArray[i] ] – 1] = inputArray[i].
            Also, update countArray[ inputArray[i] ] = countArray[ inputArray[i] ]– -.

        Counting Sort Algorithm:
        Declare an auxiliary array countArray[] of size max(inputArray[])+1 and initialize it with 0.
        Traverse array inputArray[] and map each element of inputArray[] as an index of countArray[] array,
                i.e., execute countArray[inputArray[i]]++ for 0 <= i < N.
        Calculate the prefix sum at every index of array inputArray[].
        Create an array outputArray[] of size N.
        Traverse array inputArray[] from end and update outputArray[ countArray[ inputArray[i] ] – 1] = inputArray[i].
                Also, update countArray[ inputArray[i] ] = countArray[ inputArray[i] ]- – .
   */

  def countSort(inputArray: Array[Int]): Array[Int] = {
    val N = inputArray.length
    val M = inputArray.max
    val countArray = new Array[Int](M + 1)
    for (i <- 0 until N) countArray(inputArray(i)) += 1
    for (i <- 1 to M) countArray(i) += countArray(i - 1)
    val outputArray = new Array[Int](N)
    for (i <- N - 1 to 0 by -1) {
      outputArray(countArray(inputArray(i)) - 1) = inputArray(i)
      countArray(inputArray(i)) -= 1
    }
    outputArray
  }

  val inputArray = Array(4, 3, 12, 1, 5, 5, 3, 9)
  val outputArray = countSort(inputArray)
  outputArray.foreach(println)
}
