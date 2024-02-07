package org.algorithms.bitmanipulations

object CounttotalsetbitsinfirstNNaturalNumbers extends App {
  /**
   * https://www.geeksforgeeks.org/count-total-set-bits-in-all-numbers-from-1-to-n/
   * Count total set bits in first N Natural Numbers
   *
   * Given a positive integer N, the task is to count the total number of set bits in binary representation of all
   * numbers from 1 to N.
   */

  def countSetBitsUtil(x: Int): Int = {
    if (x <= 0) 0
    else (if (x % 2 == 0) 0 else 1) + countSetBitsUtil(x / 2)
  }
  def countSetBits(n: Int): Int = {
    (1 to n).map(countSetBitsUtil(_)).sum
  }

  print(countSetBits(4))
}
