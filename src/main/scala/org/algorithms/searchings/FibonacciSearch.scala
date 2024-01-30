package org.algorithms.searchings

import scala.math.min
object FibonacciSearch extends App {
  /**
   * https://www.geeksforgeeks.org/fibonacci-search/
   * Fibonacci Search
   *
   * Given a sorted array arr[] of size n and an element x to be searched in it. Return index of x if it is
   * present in array else return -1.
   * Algorithm:
    Let the searched element be x.
    The idea is to first find the smallest Fibonacci number that is greater than or equal to the length of the given array.
    Let the found Fibonacci number be fib (m’th Fibonacci number). We use (m-2)’th Fibonacci number as the index
    (If it is a valid index). Let (m-2)’th Fibonacci Number be i, we compare arr[i] with x, if x is same, we return i.
    Else if x is greater, we recur for subarray after i, else we recur for subarray before i.

    Below is the complete algorithm
    Let arr[0..n-1] be the input array and the element to be searched be x.
    Find the smallest Fibonacci Number greater than or equal to n. Let this number be fibM [m’th Fibonacci Number].
    Let the two Fibonacci numbers preceding it be fibMm1 [(m-1)’th Fibonacci Number] and fibMm2 [(m-2)’th Fibonacci
    Number].
    While the array has elements to be inspected:
        Compare x with the last element of the range covered by fibMm2
        If x matches, return index
        Else If x is less than the element, move the three Fibonacci variables two Fibonacci down, indicating
          elimination of approximately rear two-third of the remaining array.
        Else x is greater than the element, move the three Fibonacci variables one Fibonacci down. Reset offset to
          index. Together these indicate the elimination of approximately front one-third of the remaining array.
    Since there might be a single element remaining for comparison, check if fibMm1 is 1. If Yes, compare x with that
       remaining element. If match, return index.

   */

  /* Returns index of x if present, else returns -1 */
  def fibMonaccianSearch(arr: Array[Int], x: Int, n: Int): Int = {
    /* Initialize fibonacci numbers */
   /* var fibMMm2 // (m-2)'th Fibonacci No.
    var fibMMm1 // (m-1)'th Fibonacci No.
    var fibM: Int = fibMMm2 + fibMMm1 // m'th Fibonacci*/
    /* fibM is going to store the smallest  Fibonacci Number greater than or equal to n */
    def fibNumbers(m1: Int, m2: Int, fibm: Int) :(Int, Int, Int)={
      if (fibm >= n) (m1, m2, fibm)
      else fibNumbers(fibm, m1, m1 + m2)
    }

    def fibSearch(m1: Int, m2: Int, fm: Int, offset: Int): Int = {
      if (fm>1){
         // Check if fibMm2 is a valid location
          val i: Int = min(offset + m2, n - 1)
          /* If x is greater than the value at
                      index fibMm2, cut the subarray array
                      from offset to i */
          if (arr(i) < x) {
            fibSearch(m2, fm - m1, m1, i)
          }
          else if (arr(i) > x) {
              fibSearch(m1 - m2, fm - m1, m2, offset)
            }
            else {
               i
            }
        }else if (m1 == 1 && arr(n - 1) == x) {
          n - 1
        }else -1

    }
    //find largest fib numbers
    val(fibMMm1, fibMMm2,  fibM) = fibNumbers(1, 0, 1)
    fibSearch(fibMMm1, fibMMm2, fibM, -1)
  }

  val arr = Array(10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100, 235)
  val n = 12
  val x = 235
  val ind = fibMonaccianSearch(arr, x, n)
  println(if (ind >0) s"Found at index: $ind" else "Not found!")
}
