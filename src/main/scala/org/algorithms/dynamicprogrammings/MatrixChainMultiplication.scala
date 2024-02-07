package org.algorithms.dynamicprogrammings

object MatrixChainMultiplication extends App {
  /**
   * https://www.geeksforgeeks.org/matrix-chain-multiplication-dp-8/
   * Matrix Chain Multiplication | DP-8
   *
   * Given the dimension of a sequence of matrices in an array arr[], where the dimension of the ith matrix is
   * (arr[i-1] * arr[i]), the task is to find the most efficient way to multiply these matrices together such that
   * the total number of element multiplications is minimum.
      Examples:
      Input: arr[] = {40, 20, 30, 10, 30}
      Output: 26000
      Explanation:There are 4 matrices of dimensions 40×20, 20×30, 30×10, 10×30.
      Two matrices of size m*n and n*p when multiplied, they generate a matrix of size m*p and the number of
      multiplications performed are m*n*p.
   */

  def MatrixChainOrder(p: Array[Int], i: Int, j: Int): Int = {
    if (i == j) 0
    else {
      var min = Integer.MAX_VALUE
      for (k <- i until j) {
       min = math.min(min,  MatrixChainOrder(p, i, k) + MatrixChainOrder(p, k + 1, j) + p(i - 1) * p(k) * p(j))
      }
      min
    }
  }

  val arr = Array[Int](1, 2, 3, 4, 3)
  val N = arr.length
  System.out.println("Minimum number of multiplications is " + MatrixChainOrder(arr, 1, N - 1))
}
