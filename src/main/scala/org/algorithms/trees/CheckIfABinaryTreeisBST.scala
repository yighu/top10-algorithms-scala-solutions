package org.algorithms.trees

object CheckIfABinaryTreeisBST extends App {
  /**
   * https://www.geeksforgeeks.org/a-program-to-check-if-a-binary-tree-is-bst-or-not/
   * check if a Binary Tree is BST or not
   *
   * A Binary Search Tree (BST) is a node-based binary tree data structure that has the following properties.
      The left subtree of a node contains only nodes with keys less than the node’s key.
      The right subtree of a node contains only nodes with keys greater than the node’s key.
      Both the left and right subtrees must also be binary search trees.
      Each node (item in the tree) has a distinct key.
   */
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])
  def isBst(nodeOpt: Option[TreeNode], mn: Int, mx: Int): Boolean = {
    nodeOpt match {
      case None => true
      case Some(node) => {
        if (node.value < mn || node.value > mx)  false
        else isBst(node.left, mn, node.value -1) && isBst(node.right, node.value+1, mx)
      }
    }
  }

  val tree = TreeNode(4, Some(TreeNode(2, Some(TreeNode(1, None, None)), Some(TreeNode(3, None, None)))), Some(TreeNode(5, None, None)))
  val result = isBst(Some(tree),Int.MinValue, Int.MaxValue)
  println(s"$tree ")
  println(s"is BST? $result")
}
