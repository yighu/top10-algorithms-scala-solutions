package org.algorithms.bitmanipulations

object UniqueNumbers2 extends App {
  /**
   * https://www.geeksforgeeks.org/find-two-non-repeating-elements-in-an-array-of-repeating-elements/
   * Find the two non-repeating elements in an array of repeating elements/ Unique Numbers 2
   * Given an array in which all numbers except two are repeated once. (i.e. we have 2n+2 numbers and n numbers are
   * occurring twice and the remaining two have occurred once). Find those two numbers in the most efficient way.
   *
   */

  def get2NonRepeatingNos(nums: List[Int]):List[Int] = {
    nums.groupBy(identity).view.mapValues(_.length).filter(p => p._2 == 1).take(2).map(_._1).toList
  }

  val arr = List(2, 3, 7, 9, 11, 2, 3, 11)
  val uniq2= get2NonRepeatingNos(arr)
  println(uniq2)
}
