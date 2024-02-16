package org.algorithms.dynamicprogrammings

object NewYearChaos extends App {
  /**
   *
   */

  def minimumBribes(raw: Array[Int]) = {
    var swapCount = 0
    var arr = raw
    var done = false
    for ( i <- arr.length-1 to 0 by -1 if !done){
      if (arr(i) != i+1) { //filter cases, when you do not bribem be at the ith position whereever you are
        if (((i-1) >= 0) && arr(i-1) == (i+1)){  //1. Being at initial ith position, valid current position after given bribe can be (i-1)th position
          swapCount += 1
          arr = swap(arr, i, i-1)
        }else if (((i-2 >=0 && arr(i-2) == (i+1)))){//2. Beinig at initial ith position. Valid position after given bribes can be (i-2)th position
          swapCount += 2
          arr = swap(arr, i-2, i-1)
          arr = swap(arr, i-1 , i)
        }else { //3. Too chaotic(trying to bribe more than 2 people ahead of you
          println("Too Chaos!")
          done = true
        }
      }
    }
    println(s" swapped $swapCount")
  }

  def swap(arr: Array[Int], a: Int, b: Int): Array[Int] = {
    val tm = arr(a)
    arr(a) = arr(b)
    arr(b) = tm
    arr
  }

  minimumBribes(Array(2,1,5,3,4))
}
