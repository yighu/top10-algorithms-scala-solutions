package org.algorithms.searchings

import scala.+:

object DepthFirstSearch extends App {
  /**
   * https://www.geeksforgeeks.org/depth-first-search-or-dfs-for-a-graph/
   * Depth First Search
   *
   * Depth-first search is an algorithm for traversing or searching tree or graph data structures. The algorithm
   * starts at the root node (selecting some arbitrary node as the root node in the case of a graph) and explores as
   * far as possible along each branch before backtracking.
   */
  class Graph() {
    var adjacencyList: Map[Int, List[Int]] = Map()
    def addEdge(src: Int, dest: Int): Unit = {
      adjacencyList += (src -> (dest :: adjacencyList.getOrElse(src, Nil)))
    }

    def dfs(startVertex: Int): List[Int] = {
      var histroy: List[Int] = List()

      def dfsUtil(vertex: Int, visited: Set[Int]): List[Int] = {
        if (visited(vertex)) {
          Nil
        } else {
          histroy = histroy :+ vertex
          adjacencyList.getOrElse(vertex, Nil).flatMap(nextVertex => dfsUtil(nextVertex, visited + vertex))
        }
      }

      dfsUtil(startVertex, Set())
      histroy
    }
  }

   /* val graph = new Graph(7)

    graph.addEdge(0, 1)
    graph.addEdge(0, 2)
    graph.addEdge(1, 3)
    graph.addEdge(1, 4)
    graph.addEdge(2, 5)
    graph.addEdge(2, 6)*/

  val g = new Graph()

  g.addEdge(0, 1)
  g.addEdge(0, 2)
  g.addEdge(1, 2)
  g.addEdge(2, 0)
  g.addEdge(2, 3)

  println("Depth First Traversal starting from vertex 0:")
  g.dfs(2).foreach(println)


}
