package org.algorithms.greedys


object ActivitySelection extends App {
  /**
   *
   * https://www.geeksforgeeks.org/activity-selection-problem-greedy-algo-1/
   * Activity Selection
   * You are given n activities with their start and finish times. Select the maximum number of activities that
   * can be performed by a single person, assuming that a person can only work on a single activity at a time.
   *
   * The greedy choice is to always pick the next activity whose finish time is the least among the remaining
   * activities and the start time is more than or equal to the finish time of the previously selected activity. We
   * can sort the activities according to their finishing time so that we always consider the next activity as the
   * minimum finishing time activity
   *
   * Follow the given steps to solve the problem:

      Sort the activities according to their finishing time
      Select the first activity from the sorted array and print it
      Do the following for the remaining activities in the sorted array
          If the start time of this activity is greater than or equal to the finish time of the previously selected
          activity then select this activity and print it
   */

  case class Activity(start: Int, finish: Int)


  def printMaxActivities(raw: Array[Activity], n: Int): Unit = {
    // Sort jobs according to finish time
    val arr = raw.sortWith((a,b)=>a.finish<b.finish)
    System.out.println("Following activities are selected :")
    // The first activity always gets selected
    var i = 0
    System.out.print("(" + arr(i).start + ", " + arr(i).finish + ")")
    // Consider rest of the activities
    for (j <- 1 until n) {
      // If this activity has start time greater than or equal to the finish time of previously selected activity,
      // then select it
      if (arr(j).start >= arr(i).finish) {
        System.out.print(", (" + arr(j).start + ", " + arr(j).finish + ")")
        i = j
      }
    }
  }

  val n = 6
  val arr = new Array[Activity](n)
  arr(0) = Activity(5, 9)
  arr(1) = Activity(1, 2)
  arr(2) = Activity(3, 4)
  arr(3) = Activity(0, 6)
  arr(4) = Activity(5, 7)
  arr(5) = Activity(8, 9)

  // Function call
  printMaxActivities(arr, n)

}
