package org.algorithms.recursionbacktracking

object RatInAMaze extends App {
  /**
   * https://www.geeksforgeeks.org/rat-in-a-maze/
   * Rat in a Maze
   * Consider a rat placed at (0, 0) in a square matrix of order N * N. It has to reach the destination at
   * (N – 1, N – 1). Find all possible paths that the rat can take to reach from source to destination. The directions
   * in which the rat can move are ‘U'(up), ‘D'(down), ‘L’ (left), ‘R’ (right). Value 0 at a cell in the matrix
   * represents that it is blocked and rat cannot move to it while value 1 at a cell in the matrix represents that rat
   * can be travel through it. Return the list of paths in lexicographically increasing order.
   *  Note: In a path, no cell can be visited more than one time. If the source cell is 0, the rat cannot move to any
   *  other cell.
   *  Rat in a Maze using Backtracking:
   *  We use a backtracking algorithm to explore all possible paths. While exploring the paths we keep track of the
   *  directions we have moved so far and when we reach to the bottom right cell, we record the path in a vector of
   *  strings.
   *
   *  Step-by-step approach:
   *
   *  Create isValid() function to check if a cell at position (r, c) is inside the maze and unblocked.
   *  Create findPath() to get all valid paths:
   *  Base case: If the current position is the bottom-right cell, add the current path to the result and return.
   *  Mark the current cell as blocked.
   *  Iterate through all possible directions.
   *  Calculate the next position based on the current direction.
   *  If the next position is valid (i.e, if isValid() return true), append the direction to the current path
   *  and recursively call the findPath() function for the next cell.
   *  Backtrack by removing the last direction from the current path.
   *  Mark the current cell as unblocked before returning.
   */

    def solveMaze(maze: Array[Array[Int]]): List[(Int, Int)] = {
      val n = maze.length
      val result = Array.ofDim[Int](n, n)
      if (solveMazeUtil(maze, 0, 0, result)) {
        result.map(_.zipWithIndex.collect { case (value, index) if value == 1 => (index, result.indexOf(result.find(row => row(index) == 1).getOrElse(Array.empty))) }).toList.flatten
      } else {
        List()
      }
    }

    def isSafe(maze: Array[Array[Int]], x: Int, y: Int): Boolean = {
      val n = maze.length
      x >= 0 && x < n && y >= 0 && y < n && maze(x)(y) == 1
    }

    def solveMazeUtil(maze: Array[Array[Int]], x: Int, y: Int, result: Array[Array[Int]]): Boolean = {
      val n = maze.length

      if (x == n - 1 && y == n - 1) {
        result(x)(y) = 1
        true
      } else if (isSafe(maze, x, y)) {
        result(x)(y) = 1

        if (solveMazeUtil(maze, x + 1, y, result) || solveMazeUtil(maze, x, y + 1, result)) true
        else {
          result(x)(y) = 0
          false
        }
      } else false
    }

      val maze = Array(
        Array(1, 0, 0, 0),
        Array(1, 1, 0, 1),
        Array(0, 1, 0, 0),
        Array(1, 1, 1, 1)
      )

      val solution = solveMaze(maze)

      if (solution.nonEmpty) {
        println("Path found:")
        solution.foreach(println)
      } else {
        println("No path found.")
      }


}