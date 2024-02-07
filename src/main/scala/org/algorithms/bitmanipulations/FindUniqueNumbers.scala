package org.algorithms.bitmanipulations

object FindUniqueNumbers extends App {
  /**
   * https://www.geeksforgeeks.org/find-element-appears-array-every-element-appears-twice/
   * Find the element that appears once in an array where every other element appears twice
   */

  def findUniques(arr: List[Int]): List[Int] = {
    arr.groupBy(identity).view.mapValues(_.length).filter(_._2 == 1).map(_._1).toList
  }

  val ar = List(2, 3, 5, 4, 5, 3, 4)
  val uniques = findUniques(ar)
  println(uniques)
}
