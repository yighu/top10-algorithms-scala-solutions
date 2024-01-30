package org.algorithms.searchings

object BreadthFirstSearch extends App {
  /**
   * https://www.geeksforgeeks.org/breadth-first-search-or-bfs-for-a-graph/
   * Breadth First Search
   * The Breadth First Search (BFS) algorithm is used to search a graph data structure for a node that meets a set of
   * criteria. It starts at the root of the graph and visits all nodes at the current depth level before moving on to
   * the nodes at the next depth level.
   *
   * How does BFS work?
     Starting from the root, all the nodes at a particular level are visited first and then the nodes of the next level are
     traversed till all the nodes are visited.

     To do this a queue is used. All the adjacent unvisited nodes of the current level are pushed into the queue and
     the nodes of the current level are marked visited and popped from the queue.

   */

  import scala.collection.mutable.Queue

  class Graph() {
    var adjacencyList: Map[Int, List[Int]] = Map()

    def addEdge(v: Int, w: Int): Unit = {
      adjacencyList = adjacencyList + (v -> (w :: adjacencyList.getOrElse(v, Nil)))
    }

    def breadthFirstSearch(startVertex: Int): List[Int] = {
      var visited: List[Int] = List()
      val queue: Queue[Int] = Queue()



      def visitNeighbour(neighbours: List[Int]): Unit = {
        neighbours match {
          case Nil =>
          case neighbor :: rest =>  {
            if (!visited.contains(neighbor)) {
              visited = visited :+ neighbor
              queue.enqueue(neighbor)
            }
            visitNeighbour(rest)
          }
        }
      }
      def cleanQueue(): Unit={
        if (queue.nonEmpty) {
          val currentVertex = queue.dequeue()
          visitNeighbour(adjacencyList.getOrElse(currentVertex, Nil))
          cleanQueue()
        }
      }
      //get started
      visited = visited :+ startVertex
      queue.enqueue(startVertex)
      //finish the rest
      cleanQueue()

      visited
    }
  }

  // Example usage
  val graph = new Graph()
  graph.addEdge(0, 1)
  graph.addEdge(0, 2)
  graph.addEdge(1, 2)
  graph.addEdge(2, 0)
  graph.addEdge(2, 3)
  graph.addEdge(3, 3)

  val bfsResult = graph.breadthFirstSearch(2)
  println(s"BFS starting from vertex 2: $bfsResult")

}
