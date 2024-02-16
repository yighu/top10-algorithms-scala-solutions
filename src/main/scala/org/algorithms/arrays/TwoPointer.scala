package org.algorithms.arrays

object TwoPointer extends App {
  /**
   * Opposite directional start<------>end
   * Equi-directional  ->start---end
   */

  //Opposite directional
  //Two sum II -input sorted
def twoSum(nums: List[Int], target: Int): Array[Int] = {
    var start = 0
    var end = nums.length-1
    val result = Array.ofDim[Int](2)
    while(start<end){
      val sum = nums(start) + nums(end)
      if (sum == target){
        result(0) = start+1
        result(1) = end+1
        start=end //to break the loop
      }else if (sum<target){
        start += 1
      }else {
        end -= 1
      }
    }
    result
  }//O(n)
//Equi-directional

  def windowSum(nums: List[Int], k: Int): Int = {  //windowsum += A[end] - A[start]
    var (start, end, maxSum) = (0,0,0)


    var windoSum = nums.slice(start,k).sum
    while(end<nums.length-k){
      windoSum += nums(end+k) - nums(start)
      start += 1
      end += 1
      maxSum = math.max(maxSum , windoSum)
    }
    maxSum
  }//O(n)

    val nums = List(-3,2,3,3,6,8,15)
  twoSum(nums, 6) foreach(println)
  val input = List(2,1,5,1,3,2)
  println(windowSum(input, 3))
}
