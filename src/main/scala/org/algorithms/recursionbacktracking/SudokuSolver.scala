package org.algorithms.recursionbacktracking

object SudokuSolver extends App {
  /**
   * https://www.geeksforgeeks.org/sudoku-backtracking-7/
   * Sudoku Solver
   * Given a partially filled 9×9 2D array ‘grid[9][9]’, the goal is to assign digits (from 1 to 9) to the empty
   * cells so that every row, column, and subgrid of size 3×3 contains exactly one instance of the digits from 1 to 9.

      Sudoku using Backtracking:
      Like all other Backtracking problems, Sudoku can be solved by assigning numbers one by one to empty cells.
      Before assigning a number, check whether it is safe to assign.

      Check that the same number is not present in the current row, current column and current 3X3 subgrid. After
      checking for safety, assign the number, and recursively check whether this assignment leads to a solution or not.
      If the assignment doesn’t lead to a solution, then try the next number for the current empty cell. And if none of
      the number (1 to 9) leads to a solution, return false and print no solution exists.

      Follow the steps below to solve the problem:

      Create a function that checks after assigning the current index the grid becomes unsafe or not. Keep Hashmap
        for a row, column and boxes. If any number has a frequency greater than 1 in the hashMap return false else
        return true; hashMap can be avoided by using loops.
      Create a recursive function that takes a grid.
      Check for any unassigned location.
        If present then assigns a number from 1 to 9.
        Check if assigning the number to current index makes the grid unsafe or not.
        If safe then recursively call the function for all safe cases from 0 to 9.
        If any recursive call returns true, end the loop and return true. If no recursive call returns true then
          return false.
      If there is no unassigned location then return true.
       */


  def isSafe(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = { // Row has the unique (row-clash)
    def numInRow(): Boolean = {
      var found = false
      for (d <- 0 until board.length if !found) { // Check if the number we are trying to
        // place is already present in
        // that row, return false;
        found = (board(row)(d) == num)
      }
    found
    }
    def numInColumn(): Boolean = {
      // Column has the unique numbers (column-clash)
      var found = false

      for (r <- 0 until board.length if !found) { // Check if the number
        // we are trying to
        // that column, return false;
        found = (board(r)(col) == num)
      }
      found
    }
    def numInSquare(): Boolean = {
      // Corresponding square has
      // unique number (box-clash)
      var found = false
      val sqrt = Math.sqrt(board.length).toInt
      val boxRowStart = row - row % sqrt
      val boxColStart = col - col % sqrt
      for (r <- boxRowStart until boxRowStart + sqrt if !found) {
        for (d <- boxColStart until boxColStart + sqrt if !found) {
          found = (board(r)(d) == num)
        }
      }
      found
    }
    if (numInRow() || numInColumn() || numInSquare()) false
    else true
  }

  def solveSudoku(board: Array[Array[Int]], n: Int): Boolean = {
    def boardContainsEmpty(): (Boolean, Int, Int) = {
          var row: Int = -1
          var col: Int = -1
          var notEmpty: Boolean = true
          for (i <- 0 until n if notEmpty) {
              val emptyIndex = board(i).indexOf(0)
              if (emptyIndex >= 0){
                row = i
                col = emptyIndex
                notEmpty = false
              }
          }
        (notEmpty, row, col)
    }
    val (notEmpty, row, col) = boardContainsEmpty()
    notEmpty match {
      case true => true
      case _ => {
        // Else for each-row backtrack
        def isSolved(): Boolean = {
          var solved = false
          for (num <- 1 to n if !solved) {
            if (isSafe(board, row, col, num)) {
              board(row)(col) = num
              if (solveSudoku(board, n)) { // print(board, n);
                solved = true
              }
              else { // replace it
                board(row)(col) = 0
              }
            }
          }
          solved
        }
        if (isSolved()) true
        else false
      }
    }

  }

  def print(board: Array[Array[Int]], N: Int): Unit = { // We got the answer, just print it
    ( 0 until N) map {r =>
       (0 until N).map(d => board(r)(d)).mkString(" ")
    } foreach(println)
  }

  val board = Array[Array[Int]](Array(3, 0, 6, 5, 0, 8, 4, 0, 0), Array(5, 2, 0, 0, 0, 0, 0, 0, 0), Array(0, 8, 7, 0, 0, 0, 0, 3, 1), Array(0, 0, 3, 0, 1, 0, 0, 8, 0), Array(9, 0, 0, 8, 6, 3, 0, 0, 5), Array(0, 5, 0, 0, 9, 0, 6, 0, 0), Array(1, 3, 0, 0, 0, 0, 2, 5, 0), Array(0, 0, 0, 0, 0, 0, 0, 7, 4), Array(0, 0, 5, 2, 0, 6, 3, 0, 0))
  val N = board.length

  if (solveSudoku(board, N)) { // print solution
    print(board, N)
  }
  else println("No solution")
}
