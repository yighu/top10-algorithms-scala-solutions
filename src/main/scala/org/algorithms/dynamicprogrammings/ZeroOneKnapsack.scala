package org.algorithms.dynamicprogrammings

object ZeroOneKnapsack extends App {
  /**
   *0/1 Knapsack Problem
   * Given N items where each item has some weight and profit associated with it and also given a bag with capacity W,
   * [i.e., the bag can hold at most W weight in it]. The task is to put the items into the bag such that the sum of
   * profits associated with them is the maximum possible.
   *
   */

  def knapSack(W: Int, wt: Array[Int], profts: Array[Int], n: Int): Int = {
    if (n == 0 || W == 0)  0
    else if (wt(n - 1) > W) knapSack(W, wt, profts, n - 1)
    else {
       math.max(profts(n - 1) + knapSack(W - wt(n - 1), wt, profts, n - 1), knapSack(W, wt, profts, n - 1))
    }
  }

  val profit = Array[Int](60, 100, 120)
  val weight = Array[Int](10, 20, 30)
  val W = 50
  val n = profit.length
  System.out.println(knapSack(W, weight, profit, n))
}
