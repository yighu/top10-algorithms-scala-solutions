package org.algorithms.bitmanipulations

object LengthLongest1sinNumberBinRep extends App {
  /**
   * Length Of The Longest Consecutive 1s In Binary Representation Of A Number | BitManipulation
   */
val number = 211184
//step => 1
  val bin = "110011100011110000"   // 2 3 4
//shift
  (number<<1)
   "110011100011110000"
  "110011100011110000"
  number & (number<<1)
//step 2
   "110011100011110000"
  "110011100011110000"
   "10001100001110000"          //(1 2 3)
  number & (number<<1)
  //step 3
   "00000100000110000"          //(0 1 2)
  number & (number<<1)
  "00000000000010000"          //(0 0 1)
  //step 4
  number & (number<<1)
  "00000000000000000"          //(0 0 0)

  def countConsecutive1sInBinary( n: Int) : Int ={
    def countIt(number: Int, cnt: Int): Int = {
      if (number == 0) cnt
      else countIt(number & (number << 1), cnt + 1)
    }
    countIt(n,0)
  }
  val cnt = countConsecutive1sInBinary(211184)
  println(cnt)
}
