package org.algorithms.greedys

import scala.annotation.tailrec

object MinimizeCashFlow extends App {
  /**
   * https://www.geeksforgeeks.org/minimize-cash-flow-among-given-set-friends-borrowed-money/
   * Minimize Cash Flow among a given set of friends who have borrowed money from each other
   *
   * Given a number of friends who have to give or take some amount of money from one another. Design an algorithm by
   * which the total cash flow among all the friends is minimized.
   *
   * Do following for every person Pi where i is from 0 to n-1.

      Compute the net amount for every person. The net amount for person ‘i’ can be computed by subtracting sum of all
         debts from sum of all credits.
      Find the two persons that are maximum creditor and maximum debtor. Let the maximum amount to be credited maximum
          creditor be maxCredit and maximum amount to be debited from maximum debtor be maxDebit. Let the maximum
          debtor be Pd and maximum creditor be Pc.
      Find the minimum of maxDebit and maxCredit. Let minimum of two be x. Debit ‘x’ from Pd and credit this amount
         to Pc
      If x is equal to maxCredit, then remove Pc from set of persons and recur for remaining (n-1) persons.
      If x is equal to maxDebit, then remove Pd from set of persons and recur for remaining (n-1) persons.
   */

  // Number of persons (or vertices in the graph)// Number of persons (or vertices in the graph)

  val N: Int = 3

  def getMinMax(arr: Array[Int]): (Int, Int) = {
    var minInd: Int = 0
    var maxInd: Int = 0
    for (i <- 1 until N) {
      if (arr(i) < arr(minInd)) {
        minInd = i
      }
      if (arr(i) > arr(maxInd)) {
        maxInd = i
      }
    }
    (minInd,maxInd)
  }
  // amount[p] indicates the net amount
  // to be credited/debited to/from person 'p'
  // If amount[p] is positive, then
  // i'th person will amount[i]
  // If amount[p] is negative, then
  // i'th person will give -amount[i]
  @tailrec
  def minCashFlowRec(amount: Array[Int]): Unit = { // Find the indexes of minimum and
    // maximum values in amount[]
    // amount[mxCredit] indicates the maximum amount
    // to be given (or credited) to any person .
    // And amount[mxDebit] indicates the maximum amount
    // to be taken(or debited) from any person.
    // So if there is a positive value in amount[],
    // then there must be a negative value
    /*val mxCredit: Int = getMax(amount)
    val mxDebit: Int = getMin(amount)*/
    val (mxDebit, mxCredit) = getMinMax(amount)
    // If both amounts are 0, then
    // all amounts are settled
    if (amount(mxCredit) > 0 || amount(mxDebit) > 0) {

    // Find the minimum of two amounts
    val min: Int = math.min(-(amount(mxDebit)), amount(mxCredit))
    amount(mxCredit) -= min
    amount(mxDebit) += min
    // If minimum is the maximum amount to be
    println("Person " + mxDebit + " pays " + min + " to " + "Person " + mxCredit)
    // Recur for the amount array.
    // Note that it is guaranteed that
    // the recursion would terminate
    // as either amount[mxCredit]  or
    // amount[mxDebit] becomes 0
    minCashFlowRec(amount)
  }
  }

  // Given a set of persons as graph[]
  // where graph[i][j] indicates
  // the amount that person i needs to
  // pay person j, this function
  // finds and prints the minimum
  // cash flow to settle all debts.
  def minCashFlow(graph: Array[Array[Int]]): Unit = {
    // Create an array amount[],
    // initialize all value in it as 0.
    val amount: Array[Int] = new Array[Int](N)
    // Calculate the net amount to
    // be paid to person 'p', and
    // stores it in amount[p]. The
    // value of amount[p] can be
    // calculated by subtracting
    // debts of 'p' from credits of 'p'
    for (p <- 0 until N) {
      for (i <- 0 until N) {
        amount(p) += (graph(i)(p) - graph(p)(i))
      }
    }
    minCashFlowRec(amount)
  }

    val graph: Array[Array[Int]] = Array(Array(0, 1000, 2000), Array(0, 0, 5000), Array(0, 0, 0))
    minCashFlow(graph)

}
