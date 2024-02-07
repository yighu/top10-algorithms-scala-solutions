package org.algorithms.greedys

import scala.annotation.tailrec

object MinimumNumberofPlatformsRequiredforaRailwayBusStation extends App {
  /**
   *
   * https://www.geeksforgeeks.org/minimum-number-platforms-required-railwaybus-station/
   * Minimum Number of Platforms Required for a Railway/Bus Station
   *
   * Given the arrival and departure times of all trains that reach a railway station, the task is to find the
   * minimum number of platforms required for the railway station so that no train waits. We are given two arrays
   * that represent the arrival and departure times of trains that stop.
   * Minimum Number of Platforms Required for a Railway/Bus Station using Sorting:
       The idea is to consider all events in sorted order. Once the events are in sorted order, trace the number of
       trains at any time keeping track of trains that have arrived, but not departed.

   Follow the steps mentioned below:

    Sort the arrival and departure times of trains.
    Create two pointers i=1, and j=0, and a variable to store ans and current count plat
    Run a loop while i<n and j<n and compare the ith element of arrival array and jth element of departure array.
    If the arrival time is less than or equal to departure then one more platform is needed so increase the count, i.e., plat++ and increment i
    Else if the arrival time is greater than departure then one less platform is needed to decrease the count, i.e., plat– and increment j
    Update the ans, i.e. ans = max(ans, plat).
   */

  import java.util

  def findPlatform(arr: Array[Int], dep: Array[Int], n: Int) = {
    @tailrec
    def checkOneByOne(i: Int, j:Int, plat_needed: Int, result: Int): Int = {
      if (i < n && j < n){
        // If next event in sorted order is arrival,
        // increment count of platforms needed
        if (arr(i) <= dep(j)) {
          checkOneByOne(i+1, j, plat_needed + 1, if (plat_needed+1 > result) plat_needed +1 else result )
        }
        else { // Else decrement count of platforms needed
          if (arr(i) > dep(j)) {
            checkOneByOne(i, j+1, plat_needed - 1, if (plat_needed-1 > result) plat_needed -1 else result )
          }else {
            checkOneByOne(i, j, plat_needed, if (plat_needed > result) plat_needed  else result )
          }
        }

      }else result
    }
    // Sort arrival and departure arrays
    util.Arrays.sort(arr)
    util.Arrays.sort(dep)
    // plat_needed indicates number of platforms
    // needed at a time
    checkOneByOne(1, 0, 1, 1 )
  }

  val arr = Array(900, 940, 950, 1100, 1500, 1800)
  val dep = Array(910, 1200, 1120, 1130, 1900, 2000)
  val n = arr.length
  System.out.println("Minimum Number of Platforms Required = " + findPlatform(arr, dep, n))
}
