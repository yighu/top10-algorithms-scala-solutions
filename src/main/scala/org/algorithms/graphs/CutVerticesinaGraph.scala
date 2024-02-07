package org.algorithms.graphs

object CutVerticesinaGraph extends App {
  /**
   * https://www.geeksforgeeks.org/articulation-points-or-cut-vertices-in-a-graph/
   * Articulation Points (or Cut Vertices) in a Graph
   * A vertex v is an articulation point (also called cut vertex) if removing v increases the number of connected
   * components.
   */
  import scala.collection.mutable.ListBuffer

  class Graph(vertices: Int) {
    private val adjList: Array[ListBuffer[Int]] = Array.fill(vertices)(ListBuffer())

    def addEdge(u: Int, v: Int): Unit = {
      adjList(u).append(v)
      adjList(v).append(u)
    }

    def findArticulationPoints(): List[Int] = {
      var time = 0
      val visited = Array.fill(vertices)(false)
      val disc = Array.fill(vertices)(-1)
      val low = Array.fill(vertices)(-1)
      val parent = Array.fill(vertices)(-1)
      val articulationPoints = ListBuffer[Int]()

      def dfs(u: Int): Unit = {
        var children = 0
        visited(u) = true
        disc(u) = time
        low(u) = time
        time += 1

        for (v <- adjList(u)) {
          if (!visited(v)) {
            children += 1
            parent(v) = u
            dfs(v)

            low(u) = math.min(low(u), low(v))

            if (parent(u) == -1 && children > 1) {
              articulationPoints.append(u)
            }
            if (parent(u) != -1 && low(v) >= disc(u)) {
              articulationPoints.append(u)
            }
          } else if (v != parent(u)) {
            low(u) = math.min(low(u), disc(v))
          }
        }
      }

      for (i <- 0 until vertices) {
        if (!visited(i)) {
          dfs(i)
        }
      }
      articulationPoints.toList
    }
  }

  val graph = new Graph(6)
  graph.addEdge(0, 1)
  graph.addEdge(0, 2)
  graph.addEdge(1, 2)
  graph.addEdge(1, 3)
  graph.addEdge(3, 4)
  graph.addEdge(3, 5)

  val result = graph.findArticulationPoints()
  println("Articulation Points: " + result.distinct.mkString(", "))

}
