package org.algorithms.graphs

object BellmanFord extends App {
  /**
   * https://www.geeksforgeeks.org/bellman-ford-algorithm-dp-23/
   * Bellman–Ford
   *
   * Algorithm to Find Negative Cycle in a Directed Weighted Graph Using Bellman-Ford:
      Initialize distance array dist[] for each vertex ‘v‘ as dist[v] = INFINITY.
      Assume any vertex (let’s say ‘0’) as source and assign dist = 0.
      Relax all the edges(u,v,weight) N-1 times as per the below condition:
        dist[v] = minimum(dist[v], distance[u] + weight)
      Now, Relax all the edges one more time i.e. the Nth time and based on the below two cases we can detect the
       negative cycle:
        Case 1 (Negative cycle exists): For any edge(u, v, weight), if dist[u] + weight < dist[v]
        Case 2 (No Negative cycle) : case 1 fails for all the edges.
   */

  case class Edge(src: Int =0, dest: Int = 0, weight: Int = 0)

  class Graph(var V: Int, var E: Int) // Creates a graph with V vertices and E edges
  {
    var edge = new Array[Edge](E)
    for (i <- 0 until E) {
      edge(i) = Edge()
    }

    // A class to represent a weighted edge in graph
    // The main function that finds shortest distances from
    // src to all other vertices using Bellman-Ford
    // algorithm. The function also detects negative weight cycle
    def BellmanFord(graph: Graph, src: Int): Unit = {
      val V = graph.V
      val E = graph.E
      val dist = new Array[Int](V)
      // Step 1: Initialize distances from src to all
      // other vertices as INFINITE
      for (i <- 0 until V) {
        dist(i) = Integer.MAX_VALUE
      }
      dist(src) = 0
      // Step 2: Relax all edges |V| - 1 times. A simple
      // shortest path from src to any other vertex can
      // have at-most |V| - 1 edges
      for (i <- 1 until V) {
        for (j <- 0 until E) {
          val u = graph.edge(j).src
          val v = graph.edge(j).dest
          val weight = graph.edge(j).weight
          if (dist(u) != Integer.MAX_VALUE && dist(u) + weight < dist(v)) dist(v) = dist(u) + weight
        }
      }
      // Step 3: check for negative-weight cycles. The
      // above step guarantees shortest distances if graph
      // doesn't contain negative weight cycle. If we get
      // a shorter path, then there is a cycle.
      for (j <- 0 until E) {
        val u = graph.edge(j).src
        val v = graph.edge(j).dest
        val weight = graph.edge(j).weight
        if (dist(u) != Integer.MAX_VALUE && dist(u) + weight < dist(v)) {
          println("Graph contains negative weight cycle")
          return
        }
      }
      printArr(dist, V)
    }

    // A utility function used to print the solution
    def printArr(dist: Array[Int], V: Int): Unit = {
      println("Vertex Distance from Source")
      for (i <- 0 until V) {
        println(i + "\t\t" + dist(i))
      }
    }

  }

  // A class to represent a connected, directed and weighted
  // graph
  //val V = 5 // Number of vertices in graph
  //val E = 8 // Number of edges in graph
  val graph = new Graph(5, 8)
  // add edge 0-1 (or A-B in above figure)

  graph.edge(0) = Edge(0,1,-1)

  // add edge 0-2 (or A-C in above figure)
  graph.edge(1) = Edge(0,2,4)

  // add edge 1-2 (or B-C in above figure)
  graph.edge(2) = Edge(1, 2, 3)
  // add edge 1-3 (or B-D in above figure)
  graph.edge(3) = Edge(1, 3, 2)

  // add edge 1-4 (or B-E in above figure)
  graph.edge(4) = Edge( 1, 4, 2)

  // add edge 3-2 (or D-C in above figure)
  graph.edge(5) = Edge(3,2,5)

  // add edge 3-1 (or D-B in above figure)
  graph.edge(6) = Edge(3, 1, 1)

  // add edge 4-3 (or E-D in above figure)
  graph.edge(7) = Edge(4, 3, -3)

  // Function call
  graph.BellmanFord(graph, 0)

  // Contributed by Aakash Hasija
}
