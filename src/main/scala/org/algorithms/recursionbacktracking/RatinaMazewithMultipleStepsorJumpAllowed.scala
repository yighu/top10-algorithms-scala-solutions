package org.algorithms.recursionbacktracking

object RatinaMazewithMultipleStepsorJumpAllowed extends App {
  /**
   * https://www.geeksforgeeks.org/rat-in-a-maze-with-multiple-steps-jump-allowed/
   * Rat in a Maze with multiple steps or jump allowed
   * A Maze is given as N*N binary matrix of blocks where source block is the upper left most block i.e., maze[0][0]
   * and destination block is lower rightmost block i.e., maze[N-1][N-1]. A rat starts from source and has to reach
   * destination. The rat can move only in two directions: forward and down.

     In the maze matrix, 0 means the block is dead end and non-zero number means the block can be used in the path
     from source to destination. The non-zero value of mat[i][j] indicates number of maximum jumps rat can make from
     cell mat[i][j].

     In this variation, Rat is allowed to jump multiple steps at a time instead of 1.

     Backtracking Algorithm:

    If destination is reached
        print the solution matrix
    Else
       a) Mark current cell in solution matrix as 1.
       b) Move forward/jump (for each valid steps) in horizontal direction
          and recursively check if this move leads to a solution.
       c) If the move chosen in the above step doesn't lead to a solution
           then move down and check if this move leads to a solution.
       d) If none of the above solutions work then unmark this cell as 0
           (BACKTRACK) and return false.
   *
   */


  // Maze size
  val N: Int = 4

  /* A utility function to print solution matrix   sol[N][N] */
  def printSolution(sol: Array[Array[Int]]): Unit =
     ( 0 until N) map { i => (0 until N).map(j => sol(i)(j)).mkString(" ")} foreach(println)

  /* A utility function to check if x, y is valid  index for N*N maze */
  def isSafe(maze: Array[Array[Int]], x: Int, y: Int): Boolean = {
    // if (x, y outside maze) return false
    if (x >= 0 && x < N && y >= 0 && y < N && maze(x)(y) != 0) true
    else false
  }

  /* This function solves the Maze problem using
  Backtracking. It mainly uses solveMazeUtil() to
  solve the problem. It returns false if no path
  is possible, otherwise return true and prints
  the path in the form of 1s. Please note that
  there may be more than one solutions,
  this function prints one of the feasible solutions.*/
  def solveMaze(maze: Array[Array[Int]]): Boolean = {
    val sol: Array[Array[Int]] = Array.ofDim[Int](N, N).map(row => Array.fill(N)(0))
    if (solveMazeUtil(maze, 0, 0, sol) == false) {
      System.out.printf("Solution doesn't exist")
      false
    } else {
      printSolution(sol)
      true
   }
  }

  /* A recursive utility function to solve Maze problem */
  def solveMazeUtil(maze: Array[Array[Int]], x: Int, y: Int, sol: Array[Array[Int]]): Boolean = {
    def checkSteps(x: Int, y: Int, i: Int): Boolean = {
      if (i <= maze(x)(y) && i < N) {
        /* Move forward in x direction */
        if (solveMazeUtil(maze, x + i, y, sol) == true) true
        /* If moving in x direction doesn't give solution then Move down in y direction */
        else if (solveMazeUtil(maze, x, y + i, sol) == true) true
        else checkSteps(x, y, i+1)
      }else false
    }

    // if (x, y is goal) return true
    if (x == N - 1 && y == N - 1) {
      sol(x)(y) = 1
      true
    }else
    // Check if maze[x][y] is valid
    if (isSafe(maze, x, y) == true) { // mark x, y as part of solution path
      sol(x)(y) = 1
      if (checkSteps(x, y, 1)) true
      else {
        sol(x)(y) = 0
        false
      }
    }else false
  }

  val maze: Array[Array[Int]] = Array(
    Array(2, 1, 0, 0),
    Array(3, 0, 0, 1),
    Array(0, 1, 0, 1),
    Array(0, 0, 0, 1)
   )
  solveMaze(maze)

}
