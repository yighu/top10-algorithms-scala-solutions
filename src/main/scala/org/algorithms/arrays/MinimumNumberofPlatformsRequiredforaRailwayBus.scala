package org.algorithms.arrays

object MinimumNumberofPlatformsRequiredforaRailwayBus extends App {
  /**
   * https://www.geeksforgeeks.org/minimum-number-platforms-required-railwaybus-station/
   * Minimum Number of Platforms Required for a Railway/Bus
   * Given the arrival and departure times of all trains that reach a railway station,
   * the task is to find the minimum number of platforms required for the railway station so that no train waits.
   * We are given two arrays that represent the arrival and departure times of trains that stop.

      Examples:

      Input: arr[] = {9:00, 9:40, 9:50, 11:00, 15:00, 18:00}, dep[] = {9:10, 12:00, 11:20, 11:30, 19:00, 20:00}
      Output: 3
      Explanation: There are at-most three trains at a time (time between 9:40 to 12:00)

      Input: arr[] = {9:00, 9:40}, dep[] = {9:10, 12:00}
      Output: 1
      Explanation: Only one platform is needed.

   Follow the steps mentioned below:

    Sort the arrival and departure times of trains.
    Create two pointers i=1, and j=0, and a variable to store ans and current count plat
    Run a loop while i<n and j<n and compare the ith element of arrival array and jth element of departure array.
    If the arrival time is less than or equal to departure then one more platform is needed so increase the count,
     i.e., plat++ and increment i
    Else if the arrival time is greater than departure then one less platform is needed to decrease the count,
     i.e., plat– and increment j
    Update the ans, i.e. ans = max(ans, plat).
   */

  import java.util

  def findPlatform(arr: Array[Int], dep: Array[Int], n: Int) = {
    // Sort arrival and departure arrays
    util.Arrays.sort(arr)
    util.Arrays.sort(dep)
    // plat_needed indicates number of platforms
    // needed at a time
    var plat_needed = 1
    var result = 1
    var i = 1
    var j = 0
    // Similar to merge in merge sort to process
    // all events in sorted order
    while ( i < n && j < n) {
      // If next event in sorted order is arrival,
      // increment count of platforms needed
      if (arr(i) <= dep(j)) {
        plat_needed += 1
        i += 1
      }
      else { // Else decrement count of platforms needed
        if (arr(i) > dep(j)) {
          plat_needed -= 1
          j += 1
        }
      }
      // Update result if needed
      result = math.max(result,plat_needed)
    }
    result
  }

  val arr = Array(900, 940, 950, 1100, 1500, 1800)
  val dep = Array(910, 1200, 1120, 1130, 1900, 2000)
  val n = arr.length
  println("Minimum Number of Platforms Required = " + findPlatform(arr, dep, n))
}
