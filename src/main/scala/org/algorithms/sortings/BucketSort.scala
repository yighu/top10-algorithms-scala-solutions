package org.algorithms.sortings

import scala.{+:, :+}
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Implicits.seqOrdering

object BucketSort extends App {
  /**
   * https://www.geeksforgeeks.org/bucket-sort-2/
   * Bucket Sort
   *
   * Bucket sort is a sorting technique that involves dividing elements into various groups, or buckets. These buckets
   * are formed by uniformly distributing the elements. Once the elements are divided into buckets, they can be sorted
   * using any other sorting algorithm. Finally, the sorted elements are gathered together in an ordered fashion.
   *
   * Bucket Sort Algorithm:
    Create n empty buckets (Or lists) and do the following for every array element arr[i].

      Insert arr[i] into bucket[n*array[i]]
      Sort individual buckets using insertion sort.
      Concatenate all sorted buckets.

   */

  def bucketSort(arr: Array[Float], n: Int): Unit = {
    val (min, max) = (arr.min, arr.max)
    val buckets = Array.fill[List[Float]](n)(List())
    //put data into bucket
    for (value <- arr) {
      val bucket_no = math.min((value - min) * n / (max - min), n - 1).toInt
      buckets(bucket_no) = buckets(bucket_no) :+ value
    }
    //sort each bucket
    for {
      i <- 0 until n
    } {
      buckets(i) = buckets(i).sorted
    }
    //put data back to ordered
    var index = 0
    for {
      i <- 0 until n
      j <- 0 until buckets(i).length
    } {
      arr(index) = buckets(i)(j)
      index += 1
    }

  }

  val arr: Array[Float] = Array(0.897.toFloat, 0.565.toFloat, 50.656.toFloat, 0.1234.toFloat, 0.665.toFloat, 50.3434.toFloat)

  val n: Int = arr.length

  bucketSort(arr, n)

  arr.foreach(println)
}
