package org.algorithms.greedys

object Connectnropeswithminimumcost extends App {
  /**
   * https://www.geeksforgeeks.org/connect-n-ropes-minimum-cost/
   * Connect n ropes with minimum cost
   * Given are N ropes of different lengths, the task is to connect these ropes into one rope with minimum cost, such
   * that the cost to connect two ropes is equal to the sum of their lengths.
   * declare a priority queue and push all the elements in it.
   * Do following while the number of elements in min-heap is greater than one.
   * Extract the minimum and second minimum from min-heap
   * Add the above two extracted values and insert the added value to the min-heap.
   * Maintain a variable for total cost and keep incrementing it by the sum of extracted values.
   * Return the value of total cost.
   */

  import java.util._

  def minCost(arr: Array[Int], n: Int): Int = {
    // Create a priority queue
    val pq = new PriorityQueue[Integer]
    // Adding items to the pQueue
    arr.foreach(pq.add(_))
    // Initialize result
    var res = 0
    // While size of priority queue
    // is more than 1
    while (pq.size > 1) { // Extract shortest two ropes from pq
      val first = pq.poll
      val second = pq.poll
      // Connect the ropes: update result
      // and insert the new rope to pq
      res += first + second
      pq.add(first + second)
    }
    res
  }


  val len = Array(4, 3, 2, 6)
  val size = len.length
  println("Total cost for connecting" + " ropes is " + minCost(len, size))
}
