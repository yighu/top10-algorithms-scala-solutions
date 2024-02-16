package org.algorithms.dynamicprogrammings

object HouseRobber extends App {
  /**
   * robe current house
   *    selected ith, then skip i-1 and i+1, but can proceed to tak i-2 or i+2
   *    ith_house_selected = currentHouseValue + rob(i-2)
   * not rob current house
   *    ith_house_not_selected = rob(i-1)
   *
   *  rob(i) = max(ith_house_selected, ith_house_not_selected)
   *
   *  T(A[], i) = {
   *     0 if i<0
   *    max(ith_house_selected, ith_house_not_selected)
   *  }
   *
   *  Given an array of positive numbers, find the maximum sum of a subsequence with the constraint that no two numbers
   *  in the sequence should be adjacent in the array
   *  ===
   *  Find the maximum sum in a array such that no two elements are adjacent
   */

   def rob(A: List[Int], i: Int): Int = {
     if (i<0) 0
     else if (A.length == 1) A(0)
     else {
       val     ith_house_selected = A(i) + rob(A,i-2)
       val ith_house_not_selected = rob(A,i-1)
       math.max(ith_house_selected, ith_house_not_selected)
     }
   }


  def robcache(A: List[Int], i: Int, cache: Array[Int]): Int = {
    if (i<0) 0
    else if (A.length == 1) A(0)
    else if (cache(i) != 0) cache(i)
    else {
      val     ith_house_selected = A(i) + robcache(A,i-2, cache)
      val ith_house_not_selected = robcache(A,i-1, cache)

      cache(i) =math.max(ith_house_selected, ith_house_not_selected)
      cache(i)
    }
  } //O(n) C(N)

  def robNoRecu(A: List[Int]): Int = {
    val cache = Array.ofDim[Int](A.length+1)
    cache(0) = A(0)
    cache(1) = math.max(A(0), A(1))
    for ( i <- 2 until A.length){
      val     ith_house_selected = A(i) + cache(i-2)
      val ith_house_not_selected = cache(i-1)
      cache(i) = math.max(ith_house_selected, ith_house_not_selected)
    }
    cache(A.length-1)
  }


  def robEfficient(A: List[Int]): Int = {
    var prev2 = A(0)
    var prev1 = math.max(A(0), A(1))
    for ( i <- 2 until A.length){
      val tp1 = prev1
      prev1 = math.max(A(i) + prev2, prev1)
      prev2 = tp1
    }
   prev1
  } //Similar to fibonacci
  val input = List(2,7,9,3,1) //Money in each house

  val out1 = 12   //2, 9 , 1  //Cannot take the immediate next house

  val input2 =List (5,3,4,11,2)
  val output2 = 16 //5, 11
  println(rob(input,input.length-1))
  println(rob(input2,input2.length-1))
  println(robcache(input,input.length-1, Array.ofDim(input.length)))
  println(robcache(input2,input2.length-1, Array.ofDim(input2.length)))

  println(robNoRecu(input))
  println(robNoRecu(input2))
}
