package org.algorithms.bitmanipulations

object NumberPowerof2 extends App {
  /**
   * https://www.geeksforgeeks.org/program-to-find-whether-a-given-number-is-power-of-2/
   * Program to find whether a given number is power of 2
   *
   */
   def isPowerOfTwo(n: Int): Boolean = {
     if (n == 0 ) false
     else math.ceil(math.log(n)/math.log(2)) == math.floor(math.log(n)/math.log(2))
   }

  assert(!isPowerOfTwo(31))
  assert(isPowerOfTwo(64))
  assert(isPowerOfTwo(2))

}
