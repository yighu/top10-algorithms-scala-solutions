package org.algorithms.trees

object TwoTreesIdentical extends App {
  /**
   * https://www.geeksforgeeks.org/write-c-code-to-determine-if-two-trees-are-identical/
   * Program to Determine if given Two Trees are Identical or not
     To identify if two trees are identical, we need to traverse both trees simultaneously, and while traversing we
     need to compare data and children of the trees
   */

  case class TreeNode(value: Int, left: Option[TreeNode] = None, right: Option[TreeNode] = None)
  def identicalTrees(a: Option[TreeNode], b: Option[TreeNode]): Boolean = {
    (a,b) match {
      case (None, None) => true
      case (_: Some[TreeNode], None) => false
      case (None, _: Some[TreeNode]) => false
      case (leftS: Some[TreeNode], rightS: Some[TreeNode]) => {
        val left = leftS.get
        val right = rightS.get
        val lv = left.value
        val rv = right.value
        lv == rv && identicalTrees(left.left, right.left) && identicalTrees(left.right, right.right)
       }
    }
  }
    val root1 = TreeNode(1, Some(TreeNode(2,Some(TreeNode(4)), Some(TreeNode(5)))), Some(TreeNode(3)))
    val root2 = TreeNode(1, Some(TreeNode(2,Some(TreeNode(4)), Some(TreeNode(5)))), Some(TreeNode(3)))
    val root3 = TreeNode(1, Some(TreeNode(2,Some(TreeNode(4)), Some(TreeNode(5)))))

  assert(identicalTrees(Some(root1), Some(root2)))
  assert(!identicalTrees(Some(root1), Some(root3)))


}
