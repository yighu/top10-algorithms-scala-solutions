package org.algorithms.trees

object DiameterofaBinaryTree extends App {
  /**
   * https://www.geeksforgeeks.org/diameter-of-a-binary-tree/
   * Diameter of a Binary Tree
   * The diameter/width of a tree is defined as the number of nodes on the longest path between two end nodes.

     The diagram below shows two trees each with a diameter of nine, the leaves that form the ends of the longest path
     are shaded (note that there is more than one path in each tree of length nine, but no path longer than nine nodes).

   */

  case class TreeNode(data:Int, left: TreeNode = null, right: TreeNode = null)
  def diameterOfBinaryTree(root: TreeNode): Int = {
    def depth(node: TreeNode): Int = {
      if (node == null) 0
      else 1 + math.max(depth(node.left), depth(node.right))
    }

    def diameter(node: TreeNode): Int = {
      if (node == null) 0
      else {
        val leftDepth = depth(node.left)
        val rightDepth = depth(node.right)

        val leftDiameter = diameter(node.left)
        val rightDiameter = diameter(node.right)

        math.max(leftDepth + rightDepth, math.max(leftDiameter, rightDiameter))
      }
    }

    diameter(root)
  }
  val root = TreeNode(1, TreeNode(2, TreeNode(4), TreeNode(5)), TreeNode(3))
  val diam = diameterOfBinaryTree(root)

  println(s"dia $diam")
}
