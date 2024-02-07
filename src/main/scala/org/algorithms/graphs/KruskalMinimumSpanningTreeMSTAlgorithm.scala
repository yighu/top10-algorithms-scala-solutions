package org.algorithms.graphs

object KruskalsMST extends App {
  /**
   * Kruskal’s Minimum Spanning Tree (MST) Algorithm
   * A minimum spanning tree (MST) or minimum weight spanning tree for a weighted, connected, undirected graph is a
   * spanning tree with a weight less than or equal to the weight of every other spanning tree.
   *
   * Sort all the edges in non-decreasing order of their weight.
      Pick the smallest edge. Check if it forms a cycle with the spanning tree formed so far. If the cycle is not
      formed, include this edge. Else, discard it.
      Repeat step#2 until there are (V-1) edges in the spanning tree.

   */

  import java.util
  import java.util.Comparator
    case class Edge(var src: Int, var dest: Int, var weight: Int)
    // Defines subset element structure
    case class Subset(var parent: Int, var rank: Int)

    // Function to find the MST
    private def kruskals(V: Int, edges: util.List[KruskalsMST.Edge]): Unit = {
      var j = 0
      var noOfEdges = 0
      // Allocate memory for creating V subsets
      val subsets = new Array[KruskalsMST.Subset](V)
      // Allocate memory for results
      val results = new Array[KruskalsMST.Edge](V)
      // Create V subsets with single elements
      for (i <- 0 until V) {
        subsets(i) = KruskalsMST.Subset(i, 0)
      }
      // Number of edges to be taken is equal to V-1
      while (noOfEdges < V - 1) {
        // Pick the smallest edge. And increment
        // the index for next iteration
        val nextEdge = edges.get(j)
        val x = findRoot(subsets, nextEdge.src)
        val y = findRoot(subsets, nextEdge.dest)
        // If including this edge doesn't cause cycle,
        // include it in result and increment the index
        // of result for next edge
        if (x != y) {
          results(noOfEdges) = nextEdge
          union(subsets, x, y)
          noOfEdges += 1
        }
        j += 1
      }
      // Print the contents of result[] to display the
      // built MST
      println("Following are the edges of the constructed MST:")
      var minCost = 0
      for (i <- 0 until noOfEdges) {
        println(results(i).src + " -- " + results(i).dest + " == " + results(i).weight)
        minCost += results(i).weight
      }
      println("Total cost of MST: " + minCost)
    }

    // Function to unite two disjoint sets
    private def union(subsets: Array[KruskalsMST.Subset], x: Int, y: Int): Unit = {
      val rootX = findRoot(subsets, x)
      val rootY = findRoot(subsets, y)
      if (subsets(rootY).rank < subsets(rootX).rank) subsets(rootY).parent = rootX
      else if (subsets(rootX).rank < subsets(rootY).rank) subsets(rootX).parent = rootY
      else {
        subsets(rootY).parent = rootX
        subsets(rootX).rank += 1
      }
    }

    // Function to find parent of a set
    private def findRoot(subsets: Array[KruskalsMST.Subset], i: Int): Int = {
      if (subsets(i).parent == i) subsets(i).parent
      else{
        subsets(i).parent = findRoot(subsets, subsets(i).parent)
        subsets(i).parent
    }}

    // Starting point of program execution
      val V = 4
      val graphEdges = new util.ArrayList[KruskalsMST.Edge](util.List.of(
        KruskalsMST.Edge(0, 1, 10),
        KruskalsMST.Edge(0, 2, 6),
        KruskalsMST.Edge(0, 3, 5),
        KruskalsMST.Edge(1, 3, 15),
        KruskalsMST.Edge(2, 3, 4)))
      // Sort the edges in non-decreasing order
      // (increasing with repetition allowed)
      graphEdges.sort(new Comparator[KruskalsMST.Edge]() {
        override def compare(o1: KruskalsMST.Edge, o2: KruskalsMST.Edge): Int = o1.weight - o2.weight
      })
      kruskals(V, graphEdges)
}
