package org.algorithms.graphs

object TopologicalSorting extends App {
  /**
   *
   * https://www.geeksforgeeks.org/topological-sorting/
   * Topological Sorting
   * Topological sorting for Directed Acyclic Graph (DAG) is a linear ordering of vertices such that for every
   * directed edge u-v, vertex u comes before v in the ordering.
   * Note: Topological Sorting for a graph is not possible if the graph is not a DAG.
   * The first vertex in topological sorting is always a vertex with an in-degree of 0 (a vertex with no incoming edges).
   * Create a stack to store the nodes.
   * Initialize visited array of size N to keep the record of visited nodes.
   * Run a loop from 0 till N :
   * if the node is not marked True in visited array then call the recursive function for topological sort and
   * perform the following steps:
   * Mark the current node as True in the visited array.
   * Run a loop on all the nodes which has a directed edge to the current node
   * if the node is not marked True in the visited array:
   * Recursively call the topological sort function on the node
   * Push the current node in the stack.
   * Print all the elements in the stack.
   */

  import scala.collection.mutable

  class Graph(vertices: Int) {
    val adjacencyList: Array[mutable.ListBuffer[Int]] = Array.fill(vertices)(mutable.ListBuffer[Int]())

    def addEdge(u: Int, v: Int): Unit = {
      adjacencyList(u).append(v)
    }

    def topologicalSort(): List[Int] = {
      val visited = Array.fill(vertices)(false)
      val stack = mutable.Stack[Int]()

      def dfs(node: Int): Unit = {
        visited(node) = true

        for (adjacent <- adjacencyList(node)) {
          if (!visited(adjacent)) {
            dfs(adjacent)
          }
        }

        stack.push(node)
      }

      for (v <- 0 until vertices) {
        if (!visited(v)) {
          dfs(v)
        }
      }

      stack.toList
    }
  }


  val graph = new Graph(6)
  graph.addEdge(5, 2)
  graph.addEdge(5, 0)
  graph.addEdge(4, 0)
  graph.addEdge(4, 1)
  graph.addEdge(2, 3)
  graph.addEdge(3, 1)

  val result = graph.topologicalSort()
  println("Topological Sorting: " + result.mkString(" -> "))


}
