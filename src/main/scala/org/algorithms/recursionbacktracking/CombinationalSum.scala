package org.algorithms.recursionbacktracking

object CombinationalSum extends App {
  /**
   *
   * https://www.geeksforgeeks.org/combinational-sum/
   * Combinational Sum
   * Given an array of positive integers arr[] and an integer x, The task is to find all unique combinations in
   * arr[] where the sum is equal to x.
     The same repeated number may be chosen from arr[] an unlimited number of times. Elements in a
      combination (a1, a2, …, ak) must be printed in non-descending order. (ie, a1 <= a2 <= … <= ak).
      If there is no combination possible print “Empty”.
     Recursively find all combinations and if the current combination sums up to give X then add this combination
     in the final set of combinations.

   Follow the below steps to implement the idea:

    Sort the array arr[] and remove all the duplicates from the arr[] then create a temporary vector r. to store
     every combination and a vector of vector res.
    Recursively follow:
      If at any time sub-problem sum == 0 then add that array to the res (vector of vectors).
      Run a while loop till the sum – arr[I] is not negative and i is less than arr.size().
        Push arr[i] in r and recursively call for i and sum-arr[i] ,then increment i by 1.
        pop r[i] from back and backtrack.
   */

  import java.util
  import java.util.Collections

  def combinationSum(arr: util.ArrayList[Integer], sum: Int) = {
    val ans = new util.ArrayList[util.ArrayList[Integer]]
    val temp = new util.ArrayList[Integer]
    val set = new util.HashSet(arr)
    arr.clear()
    arr.addAll(set)
    Collections.sort(arr)
    findNumbers(ans, arr, sum, 0, temp)
    ans
  }

  def findNumbers(ans: util.ArrayList[util.ArrayList[Integer]], arr: util.ArrayList[Integer], sum: Int, index: Int, temp: util.ArrayList[Integer]): Unit = {
    if (sum == 0) { // Adding deep copy of list to ans
      ans.add(new util.ArrayList[Integer](temp))
    }else
    for (i <- index until arr.size) { // checking that sum does not become negative
      if ((sum - arr.get(i)) >= 0) { // adding element which can contribute to
        // sum
        temp.add(arr.get(i))
        findNumbers(ans, arr, sum - arr.get(i), i, temp)
        // removing element from list (backtracking)
        temp.remove(arr.get(i))
      }
    }
  }

  import java.util

  val arr = new util.ArrayList[Integer]

  arr.add(2)
  arr.add(4)
  arr.add(6)
  arr.add(8)

  val sum = 8

  val ans = combinationSum(arr, sum)

  // If result is empty, then
  if (ans.size == 0) {
    System.out.println("Empty")

  }

  // print all combinations stored in ans

  for (i <- 0 until ans.size) {
    System.out.print("(")
    for (j <- 0 until ans.get(i).size) {
      System.out.print(ans.get(i).get(j) + " ")
    }
    System.out.print(") ")
  }
}
