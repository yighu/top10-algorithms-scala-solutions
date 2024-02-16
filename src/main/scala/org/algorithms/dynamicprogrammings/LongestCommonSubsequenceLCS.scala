package org.algorithms.dynamicprogrammings

object LongestCommonSubsequenceLCS extends App {
  /**
   * https://www.geeksforgeeks.org/longest-common-subsequence-dp-4/
   * Longest Common Subsequence (LCS)
   * Given two strings, S1 and S2, the task is to find the length of the Longest Common Subsequence, i.e.
   * longest subsequence present in both of the strings.
   *
   */
  def lcs(X: String, Y: String, m: Int, n: Int): Int = {
    if (m <= 0 || n <= 0) 0
    else if (X.charAt(m - 1) == Y.charAt(n - 1)) 1 + lcs(X, Y, m - 1, n - 1)
    else Math.max(lcs(X, Y, m, n - 1), lcs(X, Y, m - 1, n))
  } //o^(M+N)

  def lcsCache(X: String, Y: String, m: Int, n: Int): Int = {
    if (m <= 0 || n <= 0) 0
    else if (cache(m)(n)!=0)cache(m)(n)
    else if (X.charAt(m - 1) == Y.charAt(n - 1)) {
      cache(m)(n) = 1 + lcsCache(X, Y, m - 1, n - 1)
      cache(m)(n)
    }
    else {
      cache(m)(n) = Math.max(lcsCache(X, Y, m, n - 1), lcsCache(X, Y, m - 1, n))
      cache(m)(n)
    }
  } //o^(M+N)

  def lcsFast(X: String, Y: String, m: Int, n: Int): Int = {
    val mem = Array.ofDim[Int](m+1,n+1)
    var result  = 0
    for ( i<- 0 to m; j <- 0 to n)
    {
      if (i==0 || j ==0) mem(i)(j) = 0
      else if (X.charAt(i-1) == Y.charAt(j-1)){
        mem(i)(j) = mem(i-1)(j-1) + 1
        result = math.max(result, mem(i)(j))
      }else {
        mem(i)(j) = 0
      }
    }
    result
  }
/*
  case 1 : character matches
  if character X[I] matches Y[J], we can match recursively for the remaining characters
   LCS(X,Y,i,j) = 1 + LCS(X,Y, i-1, j-1) if X[I]==Y[J]
  case 2: Not match
   LCS(X,Y,i,j) = MAX(LCS(X,Y, i, j-1),      LCS(X,Y, i-1, j))

  */

  def lcsDynamics(X: List[Char], Y: List[Char], i: Int, j: Int): Int = {
    if (i<=0 || j<=0) 0
    else if (X(i-1) == Y(j-1)) lcsDynamics(X,Y, i-1, j-1)+1
    else math.max(lcsDynamics(X,Y, i, j-1), lcsDynamics(X,Y, i-1, j))
  } //O(2^(m+n)

  def lcsDynamicsCache(X: List[Char], Y: List[Char], i: Int, j: Int, dp: Array[Array[Int]]): Int = {
    if (i<=0 || j<=0) 0
    else if (dp(i)(j) != 0) dp(i)(j)
    else if (X(i-1) == Y(j-1)) {
      dp(i)(j)=lcsDynamicsCache(X,Y, i-1, j-1,dp)+1
      dp(i)(j)
    }
    else {
      dp(i)(j) = math.max(lcsDynamicsCache(X,Y, i, j-1,dp), lcsDynamicsCache(X,Y, i-1, j,dp))
      dp(i)(j)
    }
  }


  def lcsDynamicsfast(X: List[Char], Y: List[Char], m: Int, n: Int): Int = {
    val dp = Array.ofDim[Int](m+1,n+1)
    for (i <- 0 to m; j <- 0 to n)
        if (i<=0 || j<=0) dp(i)(j) = 0
        else if (X(i-1) == Y(j-1)) {
          dp(i)(j)=dp(i-1)(j-1)+1
        }
        else {
          dp(i)(j) = math.max(dp(i-1)(j), dp(i)(j-1))
        }

    dp(m)(n)
  }//O(m*n) Space O(m*n)


  def lcsDynamicsfastOneArray(X: List[Char], Y: List[Char], m: Int, n: Int): Int = {
    val dp = Array.ofDim[Int](n+1)
    var temp = 0
    var prev = 0
    for (i <- 1 to m) {
      prev = 0
      for (j <- 1 to n) {
        temp = dp(j)
        if (X(i - 1) == Y(j - 1)) {
          dp(j) = dp(j - 1) + 1
        } else {
          dp(j) = math.max(dp(j), dp(j - 1))
        }
        prev = temp
      }

    }
 dp(n)
  }//O(m*n) Space O(m*n)
  val S1 = "AGGTAB"

  val S2 = "GGTXAB"
  val m = S1.length
  val n = S2.length
  val cache = Array.ofDim[Int](m+1,n+1)

  println("Length of LCS is" + " " + lcs(S1, S2, m, n))
  println("cache Length of LCS is" + " " + lcsCache(S1, S2, m, n))
  println("fast Length of LCS is" + " " + lcsFast(S1, S2, m, n))
  println("Dynamics Length of LCS is" + " " + lcsDynamics(S1.toCharArray.toList, S2.toCharArray.toList, m, n))
  println("Dynamics cache Length of LCS is" + " " + lcsDynamicsCache(S1.toCharArray.toList, S2.toCharArray.toList, m, n, Array.ofDim[Int](m+1,n+1)))
  println("fast cache Length of LCS is" + " " + lcsDynamicsfast(S1.toCharArray.toList, S2.toCharArray.toList, m, n))
  println("fast one array Length of LCS is" + " " + lcsDynamicsfastOneArray(S1.toCharArray.toList, S2.toCharArray.toList, m, n))

}
