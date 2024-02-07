package org.algorithms.arrays

object TrappingRainWater extends App{
  /**
   * https://www.geeksforgeeks.org/trapping-rain-water/
   * Given an array of N non-negative integers arr[] representing an elevation map
   * where the width of each bar is 1, compute how much water it is able to trap after raining.
   * Intuition: The basic intuition of the problem is as follows:
   *
   * An element of the array can store water if there are higher bars on the left and the right.
   * The amount of water to be stored in every position can be found by finding the heights of
   * bars on the left and right sides.
   * The total amount of water stored is the summation of the water stored in each index.
   */
  def maxWater(arr: Array[Int], n: Int) = {
    val size = n - 1
    // Let the first element be stored as
    // previous, we shall loop from index 1
    var prev = arr(0)
    // To store previous wall's index
    var prev_index = 0
    var water = 0
    // To store the water until a larger wall
    // is found, if there are no larger walls
    // then delete temp value from water
    var temp = 0
    for (i <- 1 to size) {
      // If the current wall is taller than
      // the previous wall then make current
      // wall as the previous wall and its
      // index as previous wall's index
      // for the subsequent loops
      if (arr(i) >= prev) {
        prev = arr(i)
        prev_index = i
        // Because larger or same height wall is
        // found
        temp = 0
      }
      else {
        // Since current wall is shorter than
        // the previous, we subtract previous
        // wall's height from the current wall's
        // height and add it to the water
        water += prev - arr(i)
        // Store the same value in temp as well
        // If we dont find any larger wall then
        // we will subtract temp from water
        temp += prev - arr(i)
      }
    }
    // If the last wall was larger than or equal
    // to the previous wall then prev_index would
    // be equal to size of the array (last element)
    // If we didn't find a wall greater than or equal
    // to the previous wall from the left then
    // prev_index must be less than the index
    // of the last element
    if (prev_index < size) {
      // Temp would've stored the water collected
      // from previous largest wall till the end
      // of array if no larger wall was found then
      // it has excess water and remove that
      // from 'water' var
      water -= temp
      // We start from the end of the array, so
      // previous should be assigned to the last
      // element
      prev = arr(size)
      // Loop from the end of array up to the
      // 'previous index' which would contain the
      // "largest wall from the left"
      for (i <- size to prev_index by -1) {
        // Right end wall will be definitely smaller
        // than the 'previous index' wall
        if (arr(i) >= prev) prev = arr(i)
        else water += prev - arr(i)
      }
    }
    // Return the maximum water
    water
  }

  val arr = Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)
  val N = arr.length
  print(maxWater(arr, N))
}
