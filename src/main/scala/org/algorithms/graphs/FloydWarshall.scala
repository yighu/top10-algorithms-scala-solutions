package org.algorithms.graphs

object FloydWarshall extends App {
  /**
   * https://www.geeksforgeeks.org/floyd-warshall-algorithm-dp-16/
   * Floyd Warshall Algorithm
   *
   * Floyd Warshall Algorithm Algorithm:
    Initialize the solution matrix same as the input graph matrix as a first step.
    Then update the solution matrix by considering all vertices as an intermediate vertex.
    The idea is to pick all vertices one by one and updates all shortest paths which include the picked vertex as an
     intermediate vertex in the shortest path.
    When we pick vertex number k as an intermediate vertex, we already have considered vertices {0, 1, 2, .. k-1} as
      intermediate vertices.
    For every pair (i, j) of the source and destination vertices respectively, there are two possible cases.
        k is not an intermediate vertex in shortest path from i to j. We keep the value of dist[i][j] as it is.
        k is an intermediate vertex in shortest path from i to j. We update the value of
         dist[i][j] as dist[i][k] + dist[k][j], if dist[i][j] > dist[i][k] + dist[k][j]

   */

  val INF: Int = 99999
  val V: Int = 4

  def floydWarshall(dist: Array[Array[Int]]): Unit = {
    var i: Int = 0
    var j: Int = 0
    var k: Int = 0
    /* Add all vertices one by one
               to the set of intermediate
               vertices.
              ---> Before start of an iteration,
                   we have shortest
                   distances between all pairs
                   of vertices such that
                   the shortest distances consider
                   only the vertices in
                   set {0, 1, 2, .. k-1} as
                   intermediate vertices.
              ----> After the end of an iteration,
                    vertex no. k is added
                    to the set of intermediate
                    vertices and the set
                    becomes {0, 1, 2, .. k} */
    k = 0
    while (k < V) {
      // Pick all vertices as source one by one
      i = 0
      while (i < V) {
        // Pick all vertices as destination for the
        // above picked source
        j = 0
        while (j < V) {
          // If vertex k is on the shortest path
          // from i to j, then update the value of dist[i][j]
          dist(i)(j) = math.min(dist(i)(j), dist(i)(k) + dist(k)(j))
          j += 1
        }
        i += 1
      }
      k += 1
    }
    // Print the shortest distance matrix
    printSolution(dist)
  }

  def printSolution(dist: Array[Array[Int]]): Unit = {
    println("The following matrix shows the shortest " + "distances between every pair of vertices")
    for (i <- 0 until V) {
      for (j <- 0 until V) {
        if (dist(i)(j) == INF) {
          print("INF ")
        }
        else {
          print(dist(i)(j) + "   ")
        }
      }
      println()
    }
  }

  /* Let us create the following weighted graph
         10
      (0)------->(3)
      |         /|\
      5 |          |
      |          | 1
      \|/         |
      (1)------->(2)
         3           */
  val graph = Array(
    Array(0, 5, INF, 10),
    Array(INF, 0, 3, INF),
    Array(INF, INF, 0, 1),
    Array(INF, INF, INF, 0))
  floydWarshall(graph)
}
