package org.algorithms.greedys

object FractionalKnapsack extends App {
  /**
   * https://www.geeksforgeeks.org/fractional-knapsack-problem/
   * Fractional Knapsack
   * Given the weights and profits of N items, in the form of {profit, weight} put these items in a knapsack of
   * capacity W to get the maximum total profit in the knapsack. In Fractional Knapsack, we can break items for
   * maximizing the total value of the knapsack.

    Input: arr[] = {{60, 10}, {100, 20}, {120, 30}}, W = 50
    Output: 240
    Explanation: By taking items of weight 10 and 20 kg and 2/3 fraction of 30 kg.
    Hence total price will be 60+100+(2/3)(120) = 240

    Input:  arr[] = {{500, 30}}, W = 10
    Output: 166.667

    Fractional Knapsack Problem using Greedy algorithm:
    An efficient solution is to use the Greedy approach.
    The basic idea of the greedy approach is to calculate the ratio profit/weight for each item and sort the item on
    the basis of this ratio. Then take the item with the highest ratio and add them as much as we can (can be the
    whole element or a fraction of it).

    This will always give the maximum profit because, in each step it adds an element such that this is the maximum
    possible profit for that much weight.
    Follow the given steps to solve the problem using the above approach:

      Calculate the ratio (profit/weight) for each item.
      Sort all the items in decreasing order of the ratio.
      Initialize res = 0, curr_cap = given_cap.
      Do the following for every item i in the sorted order:
          If the weight of the current item is less than or equal to the remaining capacity then add the value of that item into the result
          Else add the current item as much as we can and break out of the loop.
      Return res.
   */


  private def getMaxValue(arr: Array[ItemValue], capacity: Int) = {
    arr.sortWith{(a,b) => a.profit/a.weight > b.profit/b.weight}
       .foldLeft((0.0,capacity)){(t: Tuple2[Double, Int], i: ItemValue) =>
          val (total, remainingcap) = t
          if (remainingcap >= i.weight) (total + i.profit, remainingcap - i.weight)
          else {
                                        val fraction = remainingcap.toDouble / i.weight
                                        (total + i.profit * fraction, (remainingcap - i.weight * fraction).toInt)
               }
       }._1
  }

  case class ItemValue(var profit: Int, var weight: Int)

  val arr = Array(ItemValue(60, 10),
                  ItemValue(100, 20),
                  ItemValue(120, 30))

  val capacity = 50

  val maxValue = getMaxValue(arr, capacity)
  System.out.println(maxValue)
}
