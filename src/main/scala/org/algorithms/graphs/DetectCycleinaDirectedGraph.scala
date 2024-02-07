package org.algorithms.graphs

import scala.collection.mutable

object DetectCycleinaDirectedGraph extends App {
  /**
   * https://www.geeksforgeeks.org/detect-cycle-in-a-graph/
   * Detect Cycle in a Directed Graph
   * Detect Cycle in a Directed Graph using DFS:
    The problem can be solved based on the following idea:

    To find cycle in a directed graph we can use the Depth First Traversal (DFS) technique. It is based on the idea
    that there is a cycle in a graph only if there is a back edge [i.e., a node points to one of its ancestors]
    present in the graph.

    To detect a back edge, we need to keep track of the nodes visited till now and the nodes that are in the current
    recursion stack [i.e., the current path that we are visiting]. If during recursion, we reach a node that is
    already in the recursion stack, there is a cycle present in the graph.
    Note: If the graph is disconnected then get the DFS forest and check for a cycle in individual trees by checking back edges.

   */

  class Graph(val V: Int) {
    val adj: Array[mutable.ListBuffer[Int]] = Array.fill(V)(mutable.ListBuffer[Int]())

    // Function to check if cycle exists
    private def isCyclicUtil(i: Int, visited: Array[Boolean], recStack: Array[Boolean]): Boolean = {
      // Mark the current node as visited and
      // part of recursion stack
      if (recStack(i)) true
      else if (visited(i)) false
      else {
        visited(i) = true
        recStack(i) = true
        val children = adj(i)
        var found = false
        for (c <- children if !found) {
          if (isCyclicUtil(c, visited, recStack)) found = true
        }
        if (found) found
        else {
          recStack(i) = false
          false
        }
      }
    }

    def addEdge(source: Int, dest: Int): Unit = {
      adj(source).append(dest)
    }

    // Returns true if the graph contains a
    // cycle, else false.
     def isCyclic: Boolean = {
      // Mark all the vertices as not visited and
      // not part of recursion stack
      val visited = new Array[Boolean](V)
      val recStack = new Array[Boolean](V)
      // Call the recursive helper function to
      // detect cycle in different DFS trees
      for (i <- 0 until V) {
        if (isCyclicUtil(i, visited, recStack)) return true
      }
      false
    }
  }

  val graph: Graph = new Graph(4)
  graph.addEdge(0, 1)
  graph.addEdge(0, 2)
  graph.addEdge(1, 2)
  graph.addEdge(2, 0)
  graph.addEdge(2, 3)
  if (graph.isCyclic) System.out.println("Graph contains cycle")
  else System.out.println("Graph doesn't " + "contain cycle")
}
