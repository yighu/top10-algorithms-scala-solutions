package org.algorithms.trees

import org.algorithms.trees.DiameterofaBinaryTree.TreeNode

object LevelOrderTraversalofBinaryTree extends App {
  /**
   * https://www.geeksforgeeks.org/level-order-tree-traversal/
   * Level Order Traversal (Breadth First Search or BFS) of Binary Tree
   *Level Order Traversal technique is defined as a method to traverse a Tree such that all nodes present in the same
   * level are traversed completely before traversing the next level.
   * How does Level Order Traversal work?
      The main idea of level order traversal is to traverse all the nodes of a lower level before moving to any of the
      nodes of a higher level. This can be done in any of the following ways:

      the naive one (finding the height of the tree and traversing each level and printing the nodes of that level)
      efficiently using a queue.
    Using Queue
     We need to visit the nodes in a lower level before any node in a higher level, this idea is quite similar to that
     of a queue. Push the nodes of a lower level in the queue. When any node is visited, pop that node from the queue
     and push the child of that node in the queue.

     This ensures that the node of a lower level are visited prior to any node of a higher level.
   */

  case class Node(data:Int, left: Node = null, right: Node = null)

  import java.util

  def printLevelOrder(): Unit = {
    val queue = new util.LinkedList[Node]
    queue.add(root)
    while (!queue.isEmpty) { // poll() removes the present head.
      val tempNode = queue.poll
      System.out.print(tempNode.data + " ")
      if (tempNode.left != null) queue.add(tempNode.left)
      if (tempNode.right != null) queue.add(tempNode.right)
    }
  }


  val root = Node(1, Node(2, Node(4), Node(5)), Node(3))
  printLevelOrder()
}
