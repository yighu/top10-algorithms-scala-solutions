package org.algorithms.recursionbacktracking

object NQueenProblem extends App {
  /**
   * https://www.geeksforgeeks.org/n-queen-problem-backtracking-3/
   * N Queen Problem
   * The N Queen is the problem of placing N chess queens on an N×N chessboard so that no two queens attack each other.
   */

  /**
   * One liner from https://contributors.scala-lang.org/t/contest-scala-lang-org-frontpage-code-snippet/1141/14
   * performance is O(n!)
   * @param n
   * @return
   */
  def nQueensx(n: Int) = (0 until n).permutations filter {p =>
    p.zipWithIndex.flatMap{case (c, d) => Seq(n + c + d, c - d)}.distinct.size == 2*n
  }
/*
  nQueensx(8).zipWithIndex foreach {case (solution, num) =>
    println(s"Solution #${num + 1}:")
    val rows = solution.map(col => solution.indices.map(i => if (i == col) 'Q' else '-').mkString)
    rows foreach println
  }*/

  val MAX = 10
  val arr = new Array[Int](MAX)  //columns of kth row
  var no = 0

  // Function to check queens placement
  //k: row#
  def nQueens(k: Int, n: Int): Unit = {
    for (i <- 1 to n) {
      if (canPlace(k, i)) {
        arr(k) = i
        if (k == n) display(n)
        else nQueens(k + 1, n)
      }
    }
  }

  // Helper Function to check if queen can be placed
  //k: row#, i: col#
  def canPlace(k: Int, i: Int): Boolean = {

    var conflict = false
    for (j <- 1 to k - 1 if !conflict) {
       conflict = arr(j) == i || (Math.abs(arr(j) - i) == Math.abs(j - k))   //conflict column or diagonal
    }
    !conflict
  }
  val line = "-" * 33
  def display(n: Int): Unit = {
    println(s"\n$line\nArrangement No. " + {no += 1; no} +s"\n$line")
    (1 to n)
      .map {i => (1 to n).map{j => if (arr(i) != j) "\t_" else "\tQ"}.mkString("")}
      .foreach(println)
  }

  val n = 5
  nQueens(1,n)

}
