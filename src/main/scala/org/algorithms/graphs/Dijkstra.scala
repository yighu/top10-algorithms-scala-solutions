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

  class Graph {
    private val vertices = mutable.Map[Int, List[(Int, Int)]]()

    def addEdge(src: Int, dest: Int, weight: Int): Unit = {
      vertices += (src -> ((dest, weight) :: vertices.getOrElse(src, Nil)))
      vertices += (dest -> ((src, weight) :: vertices.getOrElse(dest, Nil)))
    }

    def dijkstra(start: Int): Map[Int, Int] = {
      val distances = mutable.Map[Int, Int]()
      val priorityQueue = mutable.PriorityQueue[(Int, Int)]()(Ordering.by(_._2))

      vertices.keys.foreach { vertex =>
        distances += (vertex -> (if (vertex == start) 0 else Int.MaxValue))
        priorityQueue.enqueue((vertex, distances(vertex)))
      }

      while (priorityQueue.nonEmpty) {
        val currentVertex = priorityQueue.dequeue()._1

        vertices(currentVertex).foreach {
          case (neighbor, weight) =>
            val altDistance = distances(currentVertex) + weight
            if (altDistance < distances(neighbor)) {
              distances += (neighbor -> altDistance)
              priorityQueue.enqueue((neighbor, altDistance))
            }
        }
      }

      distances.toMap
    }
  }

    val graph = new Graph()

    // Adding edges to the graph
    graph.addEdge(1, 2, 1)
    graph.addEdge(1, 3, 4)
    graph.addEdge(2, 3, 2)
    graph.addEdge(2, 4, 5)
    graph.addEdge(3, 4, 1)
    graph.addEdge(4, 5, 3)

    // Running Dijkstra's algorithm starting from vertex 1
    val startVertex = 1
    val shortestDistances = graph.dijkstra(startVertex)

    println(s"Shortest distances from vertex $startVertex:")
    shortestDistances.foreach { case (vertex, distance) =>
      println(s"Vertex $vertex: $distance")
    }


}
