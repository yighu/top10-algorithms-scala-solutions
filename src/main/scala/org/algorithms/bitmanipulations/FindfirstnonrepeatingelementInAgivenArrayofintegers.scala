package org.algorithms.bitmanipulations

object FindfirstnonrepeatingelementInAgivenArrayofintegers extends App {
  /**
   * https://www.geeksforgeeks.org/non-repeating-element/
   * Find first non-repeating element in a given Array of integers
   */

  def firstNonRepeating(arr: List[Int]): Int = {
    def findFirst(remaining: List[Int], counts:Map[Int, Int]): Int = {
      remaining match {
        case Nil => -1
        case h :: tail => if (counts.getOrElse(h, -1) == 1) h else findFirst(tail, counts)
      }
    }

    val value_count=arr.groupBy(identity).view.mapValues(f => f.length).toMap
    findFirst(arr, value_count)
  }

  val arr = List(9, 4, 9, 6, 7, 4)

  print(firstNonRepeating(arr))
}
