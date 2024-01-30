package org.algorithms.searchings

object LinearSearch extends App {
  /**
   * https://www.geeksforgeeks.org/linear-search/
   * Linear Search
   *
   * In Linear Search Algorithm,

    Every element is considered as a potential match for the key and checked for the same.
    If any element is found equal to the key, the search is successful and the index of that element is returned.
    If no element is found equal to the key, the search yields “No match found”.
   */
  def search(arr: Array[Int], x: Int): Int = {
    //arr.indexOf(x)  ?? cheating?
    arr.zipWithIndex.filter(_._1 == x).map(_._2) match {
      case Array(h, _*) => h
      case _ => -1
    }
  }

  val arr = Array(2, 3, 4, 10, 40)

  // Function call
  println(search(arr,  10))
  println(search(arr, 20))
}
