package org.algorithms.trees

import org.algorithms.trees.LowestCommonAncestorinaBinaryTree.TreeNode

object MirrorTree extends App {
  /**
   * Convert a Binary Tree into its Mirror Tree (Invert Binary Tree)
   * Given a binary tree, the task is to convert the binary tree into its Mirror tree. Mirror of a Binary Tree T is
   * another Binary Tree M(T) with left and right children of all non-leaf nodes interchanged.
   *
   */
  case class TreeNode(data: Int, left: TreeNode = null, right: TreeNode = null)

  def mirror(node: TreeNode): TreeNode = {
    if (node == null) node
    else TreeNode(node.data, mirror(node.right), mirror(node.left))
  }
  val root =  TreeNode(3, TreeNode(5,
    TreeNode(6),
    TreeNode(2,
      TreeNode(7),
      TreeNode(4))),
    TreeNode(1,
      TreeNode(0),
      TreeNode(8)))

  val mirrored = mirror(root)
  val restored = mirror(mirrored)
  println(s"original: $root")
  println(s"mirrored: $mirrored")
  println(s"restored: $restored")

}
