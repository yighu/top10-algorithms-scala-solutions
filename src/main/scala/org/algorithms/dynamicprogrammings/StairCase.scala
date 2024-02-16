package org.algorithms.dynamicprogrammings

object StairCase extends App {

  def steps(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case _ => steps(n-1)+ steps(n-2)
    }

  }
  (0 to 10). map(steps(_)) foreach(println)
//Performance O(2^n)
  val cache = Map.empty[Int, Int]
  def nSteps(n: Int): Int =
    n match {
      case 0 => {
        cache + (0 -> 0)
        0
      }
      case 1 => {
        cache + (1 -> 1)
        1
      }
      case 2 => {
        cache + (2 -> 2)
        2
      }
      case _ => {
        val choice1 = cache.getOrElse(n - 1, -1)
        val choice2 = cache.getOrElse(n - 2, -1)
        (choice1, choice2) match {
          case (-1, -1) => {
            val total = steps(n - 1) + steps(n - 2)
            cache + (n -> total)
            total
          }
          case (-1, _) => steps(n - 1) + choice2
          case (_, -1) => steps(n - 2) + choice1
          case (_, _) => choice1 + choice2
        }
      }
    }

  //Performance O(N)
  (0 to 10). map(nSteps(_)) foreach(println)
//Fibonacchi
  def fSteps(n: Int): Int = {
    if (n<0) 0
    else if (n<4) n
    else fSteps(n-1) + fSteps(n-2)
  }

  (0 to 10). map(fSteps(_)) foreach(println)
//general f(n,m) = sum_1_m(f(n-m))
  //recursive => O(m^n)
  //Dynamic programming=> O(n)
 //the most efficient one
  def mSteps(n: Int): Int ={
    var (first, second, sum) = (1,1,0)
    if (n<2) n
    else {
      for( i <- 2 to n){
        sum = first + second
        first = second
        second = sum
      }
      sum
    }
  }

  def mStepsFurther(n: Int): Int ={
    var (first, second, sum) = (1,1,0)
    if (n<2) n
    else {
      for( _ <- 2 to n){
        sum = first + second
        first = second
        second = sum
      }
      sum
    }
  }

  (0 to 10). map(mSteps(_)) foreach(println)


}
