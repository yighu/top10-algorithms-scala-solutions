package org.algorithms.greedys

import scala.annotation.tailrec

object MaximumLengthChainofPairs extends App {
  /**
   * https://www.geeksforgeeks.org/maximum-length-chain-of-pairs-dp-20/
   * Maximum Length Chain of Pairs | DP-20
   *
   * You are given n pairs of numbers. In every pair, the first number is always smaller than the second number.
   * A pair (c, d) can follow another pair (a, b) if b < c. Chain of pairs can be formed in this fashion. Find the
   * longest chain which can be formed from a given set of pairs.
   */
  case class P(left: Int, right: Int)

  def maxChainLen(p:List[P], n: Int): Int = {
    val sorted = p.sortWith((a,b) => a.right < b.right)
    @tailrec
    def count(i: Int, prev: Int, ans: Int): Int = {
        if (i==n) ans
        else if (sorted(i).left > prev) count(i+1, sorted(i).right,ans+1)
        else count(i+1, prev,ans)
    }
    count(0, Int.MinValue,0)
  }

  val p = List(P(5, 24), P(39, 60), P(15, 28), P(27, 40), P(50, 90))
  System.out.println(maxChainLen(p, p.length))
}
