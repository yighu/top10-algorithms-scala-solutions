package org.algorithms.greedys

import scala.collection.JavaConverters._
object JobSequencing extends App {
  /**
   * https://www.geeksforgeeks.org/job-sequencing-problem/
   * Job Sequencing
   * Given an array of jobs where every job has a deadline and associated profit if the job is finished before the
   * deadline. It is also given that every job takes a single unit of time, so the minimum possible deadline for any
   * job is 1. Maximize the total profit if only one job can be scheduled at a time.
   * Greedy approach for job sequencing problem:
     Greedily choose the jobs with maximum profit first, by sorting the jobs in decreasing order of their profit. This
     would help to maximize the total profit as choosing the job with maximum profit for every time slot will eventually maximize the total profit

   Follow the given steps to solve the problem:

    Sort all jobs in decreasing order of profit.
    Iterate on jobs in decreasing order of profit.For each job , do the following :
        Find a time slot i, such that slot is empty and i < deadline and i is greatest.Put the job in this slot
          and mark this slot filled.
        If no such i exists, then ignore the job.
   */

  case class Job(id: Char, deadline: Int, profit: Int)

  import java.util
  import java.util.Collections
  // Function to schedule the jobs take 2 arguments// Function to schedule the jobs take 2 arguments
  // arraylist and no of jobs to schedule
  def printJobScheduling(arr: util.ArrayList[Job], t: Int): Unit = {
    // Length of array
    val n = arr.size
    // Sort all jobs according to decreasing order of profit
    Collections.sort(arr, (a: Job, b: Job) => b.profit - a.profit)

    // To keep track of free time slots
    val result = new Array[Boolean](t)
    // To store result (Sequence of jobs)
    val job = new Array[Char](t)
    // Iterate through all given jobs
    for (i <- 0 until n) {
      // Find a free slot for this job (Note that we
      // start from the last possible slot)
      var found = false
      for (j <- Math.min(t - 1, arr.get(i).deadline - 1) to 0 by -1 if !found) { // Free slot found
        if (result(j) == false) {
          result(j) = true
          job(j) = arr.get(i).id
         found = true
        }
      }
    }
    job foreach(println)
  }

  val arr :List[Job] = List(
    Job('a', 2, 100),
    Job('b', 1, 19),
    Job('c', 2, 27),
    Job('d', 1, 25),
    Job('e', 3, 15)
  )

  println("Following is maximum profit sequence of jobs")
  printJobScheduling(new java.util.ArrayList[Job](arr.asJava), 3)
}
