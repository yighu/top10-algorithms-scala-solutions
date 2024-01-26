package org.algorithms.arrays

object PrintArrayAfterItIsRightRotatedKTimes extends App{
  /**
   * https://www.geeksforgeeks.org/print-array-after-it-is-right-rotated-k-times/
   * Print array after it is right rotated K times
   * Input: Array[] = {1, 3, 5, 7, 9}, K = 2.
      Output: 7 9 1 3 5
      Explanation:
      After 1st rotation – {9, 1, 3, 5, 7}
      After 2nd rotation – {7, 9, 1, 3, 5}

      Input: Array[] = {1, 2, 3, 4, 5}, K = 4.
      Output: 2 3 4 5 1
   */

  def rightRotate(data: Array[Int], k: Int): Array[Int]={
      def takeOnetep(a: Array[Int]):Array[Int] = a.last +: a.dropRight(1)
      def rotateK(a: Array[Int], n: Int): Array[Int] = {
        n match {
          case 0 => a
          case _ => rotateK(takeOnetep(a), n-1)
        }
      }
    rotateK(data, k)
  }

  val arr = Array[Int](1, 3, 5, 7, 9, 11)
  val n = arr.length
  val k = 3 // No. of rotations
  rightRotate(arr,k).foreach(println)
}
