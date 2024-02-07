package org.algorithms.greedys

import scala.annotation.tailrec

object MinimumNumberofCoins extends App {
  /**
   * https://www.geeksforgeeks.org/greedy-algorithm-to-find-minimum-number-of-coins/
   * Minimum number of Coins
   *
   * Given a value of V Rs and an infinite supply of each of the denominations {1, 2, 5, 10, 20, 50, 100, 500, 1000}
   * valued coins/notes, The task is to find the minimum number of coins and/or notes needed to make the change?

      Examples:

      Input: V = 70
      Output: 2
      Explanation: We need a 50 Rs note and a 20 Rs note.

      Input: V = 121
      Output: 3
      Explanation: We need a 100 Rs note, a 20 Rs note, and a 1 Rs coin.

      The intuition would be to take coins with greater value first. This can reduce the total number of coins needed.
      Start from the largest possible denomination and keep adding denominations while the remaining value is greater
      than 0.

       Sort the array of coins in decreasing order.
       Initialize ans vector as empty.
       Find the largest denomination that is smaller than remaining amount and while it is smaller than the remaining
        amount:
          Add found denomination to ans. Subtract value of found denomination from amount.
        If amount becomes 0, then print ans.
   */

  val deno: Array[Int] = Array(1, 2, 5, 10, 20, 50, 100, 500, 1000)
  val n = deno.length

  def findMin(V: Int): Unit = {
    @tailrec
    def consumeCoin(i: Int, remaining: Int, acc: List[Int]): List[Int] = {
      if (i < 0) acc
      else if (remaining >= deno(i)) consumeCoin(i, remaining - deno(i), acc :+ deno(i))
      else consumeCoin(i - 1, remaining, acc)
    }

    val ans = consumeCoin(n -1, V, List())
    assert(ans.sum == V)
    ans.foreach(println)
  }

  findMin(93)

}
