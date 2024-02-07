package org.algorithms.trees

object LowestCommonAncestorinaBinaryTree extends App {
  /**
   *
   * https://www.geeksforgeeks.org/lowest-common-ancestor-binary-tree-set-1/
   * Lowest Common Ancestor in a Binary Tree
   * The lowest common ancestor is the lowest node in the tree that has both n1 and n2 as descendants, where n1 and n2
   * are the nodes for which we wish to find the LCA. Hence, the LCA of a binary tree with nodes n1 and n2 is the
   * shared ancestor of n1 and n2 that is located farthest from the root.
   *
   * Lowest Common Ancestor in a Binary Tree By Storing paths from root to n1 and root to n2:
     The idea of this approach is to store the path from the root to n1 and root to n2 in two separate data structures.
     Then look simultaneously into the values stored in the data structure, and look for the first mismatch.

      Follow the steps below to solve the problem:

      Find a path from the root to n1 and store it in a vector or array.
      Find a path from the root to n2 and store it in another vector or array.
      Traverse both paths till the values in arrays are the same. Return the common element just before the mismatch.
   */
  case class TreeNode(data: Int, left: TreeNode = null, right: TreeNode = null)
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    if (root == null || root == p || root == q) {
       root
    }else {
      val leftLCA = lowestCommonAncestor(root.left, p, q)
      val rightLCA = lowestCommonAncestor(root.right, p, q)
      if (leftLCA != null && rightLCA != null) {
         root
      } else if (leftLCA != null) {
         leftLCA
      } else {
         rightLCA
      }
    }
  }

      val root =  TreeNode(3, TreeNode(5,
                                       TreeNode(6),
                                       TreeNode(2,
                                                TreeNode(7),
                                                TreeNode(4))),
                              TreeNode(1,
                                       TreeNode(0),
                                       TreeNode(8)))


      val p = root.left
      val q = root.right

      val result = lowestCommonAncestor(root, p, q)
      println("Lowest Common Ancestor: " + result.data)

      val result1 = lowestCommonAncestor(root, root.left.left, root.left.right)
      println("expect 5, got " +result1.data)
}
