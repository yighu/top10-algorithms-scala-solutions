package org.algorithms.arrays

object Equilibriumindexofanarray extends App{
  /**
   * https://www.geeksforgeeks.org/equilibrium-index-of-an-array/
   * Equilibrium index of an array
   * Given a sequence arr[] of size n, Write a function int equilibrium(int[] arr, int n)
   * that returns an equilibrium index (if any) or -1 if no equilibrium index exists.
   * The equilibrium index of an array is an index such that the sum of elements at lower
   * indexes is equal to the sum of elements at higher indexes.

        Examples:

        Input: A[] = {-7, 1, 5, 2, -4, 3, 0}
        Output: 3
        3 is an equilibrium index, because:
        A[0] + A[1] + A[2] = A[4] + A[5] + A[6]
   */

  def equilibrium(a: Array[Int], l: Int):Int = {
    if (l == 1) 0
    else {
    val result = for {
      i <- 1 to l - 1
      left = a.slice(0, i).sum
      right = a.slice(i + 1, l).sum
      if (right == left)
    } yield i
    if (result.isEmpty) -1 else result(0)
  }
  }

  val arr = Array(-7, 1, 5, 2, -4, 3, 0)
  val arr_size = arr.length

  // Function call
  System.out.println(equilibrium(arr, arr_size))
}
