package org.algorithms.arrays

object IceCreamPalor extends App {

  /**
   * x +y = m
   * y = m-x
   *
   */
def iceCreamPalorA1(m:Int, arr: Array[Int]): Array[Int] = {
  val result = Array.ofDim[Int](2)
  var map = Map.empty[Int, Int]
  var found = false
  for ( i <- 0 until arr.length if !found){
    val x = arr(i)
    val y = m - x
    map.get(y) match {
      case None =>
      case Some(j) => {
        result(0) = j + 1
        result(1) = i + 1
        found = true
      }
    }
    map +=  (x->i)
  }
  result
}
  val m = 4
  val input = Array(1,4,5,3,2)

  println  (iceCreamPalorA1(m, input) mkString(" "))
}
