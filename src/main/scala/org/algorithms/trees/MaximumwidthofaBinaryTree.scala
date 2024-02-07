package org.algorithms.trees

object MaximumwidthofaBinaryTree extends App {
  /**
   * https://www.geeksforgeeks.org/maximum-width-of-a-binary-tree/
   * Maximum width of a Binary Tree
   */
  case class TreeNode(data: Int, left: TreeNode = null, right: TreeNode = null)

  def getMaxWidth(node: TreeNode) = {
    var maxWidth = 0
    var width = 0
    val h = height(node)
    var i = 0
    i = 1
    while (i <= h) {
      width = getWidth(node, i)
      if (width > maxWidth) maxWidth = width
      i += 1
    }
    maxWidth
  }

  def getWidth(node: TreeNode, level: Int): Int = {
    if (node == null) 0
    else if (level == 1) 1
    else if (level > 1) getWidth(node.left, level - 1) + getWidth(node.right, level - 1)
    else 0
  }

  def height(node: TreeNode): Int =
    if (node == null) 0
    else {
      val lHeight = height(node.left)
      val rHeight = height(node.right)
      math.max(lHeight, rHeight) + 1
    }


  val root =  TreeNode(3, TreeNode(5,
    TreeNode(6),
    TreeNode(2,
      TreeNode(7),
      TreeNode(4))),
    TreeNode(1,
      TreeNode(0),
      TreeNode(8)))
  val l =   getMaxWidth(root)
  println(l)
}
