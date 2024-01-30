package org.algorithms.searchings

object TernarySearch extends App {
  /**
   * https://www.geeksforgeeks.org/ternary-search/
   * Ternary Search
   * What is the Ternary Search?
      Ternary search is a searching algorithm that is used to find the position of a target value within a sorted
      array. It operates on the principle of dividing the array into three parts instead of two, as in binary search.
      The basic idea is to narrow down the search space by comparing the target value with elements at two points that
      divide the array into three equal parts.

      mid1 = l + (r-l)/3
      mid2 = r – (r-l)/3

      Working of Ternary Search:
      The concept involves dividing the array into three equal segments and determining in which segment the key
      element (the element being sought) is located. It works similarly to a binary search, with the distinction of
      reducing time complexity by dividing the array into three parts instead of two.

   What is the Ternary Search?
Ternary search is a searching algorithm that is used to find the position of a target value within a sorted array. It operates on the principle of dividing the array into three parts instead of two, as in binary search. The basic idea is to narrow down the search space by comparing the target value with elements at two points that divide the array into three equal parts.

mid1 = l + (r-l)/3
mid2 = r – (r-l)/3

Working of Ternary Search:
The concept involves dividing the array into three equal segments and determining in which segment the key element (the element being sought) is located. It works similarly to a binary search, with the distinction of reducing time complexity by dividing the array into three parts instead of two.


Below are the step-by-step explanation of working of Ternary Search:

Initialization:
    Begin with a sorted array.
    Set two pointers, left and right, initially pointing to the first and last elements of the array.
Divide the Array:
    Calculate two midpoints, mid1 and mid2, dividing the current search space into three roughly equal parts:
    mid1 = left + (right – left) / 3
    mid2 = right – (right – left) / 3
    The array is now effectively divided into [left, mid1], (mid1, mid2), and [mid2, right].
Comparison with Target:.
    If the target is equal to the element at mid1 or mid2, the search is successful, and the index is returned
    If the target is less than the element at mid1, update the right pointer to mid1 – 1.
    If the target is greater than the element at mid2, update the left pointer to mid2 + 1.
    If the target is between the elements at mid1 and mid2, update the left pointer to mid1 + 1 and the right pointer to mid2 – 1.
Repeat or Conclude:
    Repeat the process with the reduced search space until the target is found or the search space becomes empty.
    If the search space is empty and the target is not found, return a value indicating that the target is not present in the array.
   */

  def ternarySearch(l: Int, r: Int, key: Int, ar: Array[Int]): Int = {
    if (r >= l) { // Find the mid1 and mid2
      val mid1 = l + (r - l) / 3
      val mid2 = r - (r - l) / 3
      // Check if key is present at any mid
      if (ar(mid1) == key)  mid1
      else if (ar(mid2) == key)  mid2
      // Since key is not present at mid,
      // check in which region it is present
      // then repeat the Search operation
      // in that region
      else if (key < ar(mid1)) { // The key lies in between l and mid1
         ternarySearch(l, mid1 - 1, key, ar)
      }
      else if (key > ar(mid2)) { // The key lies in between mid2 and r
         ternarySearch(mid2 + 1, r, key, ar)
      }
      else { // The key lies in between mid1 and mid2
         ternarySearch(mid1 + 1, mid2 - 1, key, ar)
      }
    }else -1

  }

  val ar = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println( ternarySearch(0, 9, 5, ar))
}
