package org.algorithms.arrays
import scala.math.max
object Leadersinanarray extends App{


  /**
   * https://www.geeksforgeeks.org/leaders-in-an-array/
   * Leaders in an array
   * Write a program to print all the LEADERS in the array. An element is a leader
   * if it is greater than all the elements to its right side. And the rightmost element is always a leader.

        For example:

        Input: arr[] = {16, 17, 4, 3, 5, 2},
        Output: 17, 5, 2

        Input: arr[] = {1, 2, 3, 4, 5, 2},
        Output: 5, 2


   */

  def printLeaders(arr: Array[Int], size: Int)={
    val result = for {
      i <- 0 to size -1
      if (arr(i) > arr.slice(i+1, size).fold(0){(a,b) =>max(a,b)})
    }yield arr(i)
    result.foreach(println)

  }

  val arr = Array[Int](16, 17, 4, 3, 5, 2)
  val n = arr.length
  printLeaders(arr, n)
}
