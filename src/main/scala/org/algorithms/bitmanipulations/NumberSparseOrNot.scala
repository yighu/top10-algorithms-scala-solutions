package org.algorithms.bitmanipulations

object NumberSparseOrNot extends App {
  /**
   * Check if a given number is sparse or not
     A number is said to be a sparse number if in the binary representation of the number no two or more consecutive
     bits are set.
   */

  def checkSparse(n: Int): Int = {
    // n is not sparse if there is set in AND of n and n/2
    if ((n & (n >> 1)) >= 1) 0
    else 1
  }

  println(checkSparse(72))
  println(checkSparse(12))
  println(checkSparse(2))
  println(checkSparse(3))
}
