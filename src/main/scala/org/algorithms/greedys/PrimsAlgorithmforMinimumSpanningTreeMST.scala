package org.algorithms.greedys

object PrimsAlgorithmforMinimumSpanningTreeMST extends App {
  /**
   * https://www.geeksforgeeks.org/prims-minimum-spanning-tree-mst-greedy-algo-5/
   * Prim’s Algorithm for Minimum Spanning Tree (MST)
   *
   * The algorithm starts with an empty spanning tree. The idea is to maintain two sets of vertices. The first set
   * contains the vertices already included in the MST, and the other set contains the vertices not yet included.
   * At every step, it considers all the edges that connect the two sets and picks the minimum weight edge from these
   * edges. After picking the edge, it moves the other endpoint of the edge to the set containing MST.
   *  Step 1: Determine an arbitrary vertex as the starting vertex of the MST.
      Step 2: Follow steps 3 to 5 till there are vertices that are not included in the MST (known as fringe vertex).
      Step 3: Find edges connecting any tree vertex with the fringe vertices.
      Step 4: Find the minimum among these edges.
      Step 5: Add the chosen edge to the MST if it does not form any cycle.
      Step 6: Return the MST and exit

     Follow the given steps to utilize the Prim’s Algorithm mentioned above for finding MST of a graph:

      Create a set mstSet that keeps track of vertices already included in MST.
      Assign a key value to all vertices in the input graph. Initialize all key values as INFINITE. Assign the key
       value as 0 for the first vertex so that it is picked first.
      While mstSet doesn’t include all vertices
          Pick a vertex u that is not there in mstSet and has a minimum key value.
          Include u in the mstSet.
          Update the key value of all adjacent vertices of u. To update the key values, iterate through all adjacent vertices.
             For every adjacent vertex v, if the weight of edge u-v is less than the previous key value of v, update
             the key value as the weight of u-v.

      OPTIMIZED APPROACH OF PRIM’S ALGORITHM:
      Intuition

      We transform the adjacency matrix into adjacency list using ArrayList<ArrayList<Integer>>.
      Then we create a Pair class to store the vertex and its weight .
      We sort the list on the basis of lowest weight.
      We create priority queue and push the first vertex and its weight in the queue
      Then we just traverse through its edges and store the least weight in a variable called mstEdges.
      At last after all the vertex we return the mstEdges.

     The following code is from chat GPT
    */
  import scala.collection.mutable
  import scala.collection.mutable.ListBuffer

  class Graph(vertices: Int) {
    val adjacencyList: Array[ListBuffer[(Int, Int)]] = Array.fill(vertices)(ListBuffer())

    def addEdge(u: Int, v: Int, weight: Int): Unit = {
      adjacencyList(u).append((v, weight))
      adjacencyList(v).append((u, weight))
    }

    def primMST(): ListBuffer[(Int, Int)] = {
      val visited = Array.fill(vertices)(false)
      val minHeap = mutable.PriorityQueue[(Int, Int)]()(Ordering.by(-_._2)) //order by weight
      val mstEdges = ListBuffer[(Int, Int)]()

      // Start with the first vertex
      minHeap.enqueue((0, 0))

      while (minHeap.nonEmpty) {
        val (current, weight) = minHeap.dequeue()

        if (!visited(current)) {
          visited(current) = true
          mstEdges.append((current, weight))

          for ((neighbor, edgeWeight) <- adjacencyList(current)) {
            if (!visited(neighbor)) {
              minHeap.enqueue((neighbor, edgeWeight))
            }
          }
        }
      }

      mstEdges
    }
  }

      val graph = new Graph(5)

      graph.addEdge(0, 1, 2)
      graph.addEdge(0, 3, 1)
      graph.addEdge(1, 2, 3)
      graph.addEdge(1, 3, 2)
      graph.addEdge(2, 4, 4)
      graph.addEdge(3, 4, 5)

      // Find Minimum Spanning Tree (MST) using Prim's Algorithm
      val mstEdges = graph.primMST()

      // Print the MST edges
      println("Minimum Spanning Tree Edges:")
      for ((vertex, weight) <- mstEdges) {
        println(s"Vertex: $vertex, Weight: $weight")
      }


}
