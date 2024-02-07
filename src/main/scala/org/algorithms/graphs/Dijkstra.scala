package org.algorithms.graphs

object Dijkstra extends App {
  /**
   * https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/
   * How to find Shortest Paths from Source to all Vertices using Dijkstra’s Algorithm
   *
   * Given a weighted graph and a source vertex in the graph, find the shortest paths from the source to all the
   * other vertices in the given graph.

     Note: The given graph does not contain any negative edge.
   */
  import scala.collection.mutable

  class Graph(vertices: Int) {
    val adjacencyList: Array[mutable.ListBuffer[(Int, Int)]] = Array.fill(vertices)(mutable.ListBuffer())

    def addEdge(source: Int, destination: Int, weight: Int): Unit = {
      adjacencyList(source).append((destination, weight))
      adjacencyList(destination).append((source, weight))
    }

    def dijkstra(startNode: Int): Array[Int] = {
      val distances: Array[Int] = Array.fill(vertices)(Int.MaxValue)
      val priorityQueue: mutable.PriorityQueue[(Int, Int)] = mutable.PriorityQueue()

      distances(startNode) = 0
      priorityQueue.enqueue((startNode, 0))

      while (priorityQueue.nonEmpty) {
        val currentVertex = priorityQueue.dequeue()._1

        for (neighbor <- adjacencyList(currentVertex)) {
          val (nextVertex, edgeWeight) = neighbor
          val newDistance = distances(currentVertex) + edgeWeight

          if (newDistance < distances(nextVertex)) {
            distances(nextVertex) = newDistance
            priorityQueue.enqueue((nextVertex, newDistance))
          }
        }
      }

      distances
    }
  }

    val graph = new Graph(6)

    graph.addEdge(0, 1, 4)
    graph.addEdge(0, 2, 3)
    graph.addEdge(1, 2, 1)
    graph.addEdge(1, 3, 2)
    graph.addEdge(2, 3, 4)
    graph.addEdge(3, 4, 2)
    graph.addEdge(4, 5, 6)

    val startNode = 0
    val distances = graph.dijkstra(startNode)

    println(s"Shortest distances from node $startNode:")
    distances.zipWithIndex.foreach { case (distance, node) =>
      println(s"To node $node: $distance")
    }


}
