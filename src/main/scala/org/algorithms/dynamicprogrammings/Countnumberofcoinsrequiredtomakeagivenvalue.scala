package org.algorithms.dynamicprogrammings

object Countnumberofcoinsrequiredtomakeagivenvalue extends App {
  /**
   *https://www.geeksforgeeks.org/coin-change-dp-7/
   * Count number of coins required to make a given value
   * Given an integer array of coins[ ] of size N representing different types of denominations and an integer sum,
   * the task is to count the number of coins required to make a given value sum.
     Note: Assume that you have an infinite supply of each type of coin.

     count(coins,n,sum) = count(coins,n,sum-count[n-1]) + count(coins,n-1,sum)

     For each coin, there are 2 options.

     Include the current coin: Subtract the current coin’s denomination from the target sum and call the count
       function recursively with the updated sum and the same set of coins i.e., count(coins, n, sum – coins[n-1] )
     Exclude the current coin: Call the count function recursively with the same sum and the remaining coins. i.e.,
       count(coins, n-1,sum ).

      The final result will be the sum of both cases.

      Base case:

      If the target sum (sum) is 0, there is only one way to make the sum, which is by not selecting any coin. So,
       count(0, coins, n) = 1.
      If the target sum (sum) is negative or no coins are left to consider (n == 0), then there are no ways to make
       the sum, so count(sum, coins, 0) = 0.
   */

  def count(coins: Array[Int], n: Int, sum: Int): Int = {
    // If sum is 0 then there is 1 solution
    // (do not include any coin)
    if (sum == 0) 1
    // If sum is less than 0 or no coins then there is no solution exists
    else if (sum < 0 || n <= 0) 0
    // count is sum of solutions (i)
    // including coins[n-1] (ii) excluding coins[n-1]
    else count(coins, n - 1, sum) + count(coins, n, sum - coins(n - 1))
  }

  val coins = Array(1, 2, 3)
  val n = coins.length

  System.out.println(count(coins, n, 5))
}
