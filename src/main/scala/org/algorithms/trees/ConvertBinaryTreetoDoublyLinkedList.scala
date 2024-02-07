package org.algorithms.trees

object ConvertBinaryTreetoDoublyLinkedList extends App {
  /**
   * https://www.geeksforgeeks.org/convert-binary-tree-to-doubly-linked-list-by-keeping-track-of-visited-node/
   * Convert Binary Tree to Doubly Linked List by keeping track of visited node
   *
   * Given a Binary Tree, The task is to convert it to a Doubly Linked List keeping the same order.
    The following solution is from chatGpt
   */

  class TreeNode(var value: Int, var left: TreeNode = null, var right: TreeNode = null)

  class DoublyListNode(var value: Int, var prev: DoublyListNode = null, var next: DoublyListNode = null)

    def treeToDoublyList(root: TreeNode): DoublyListNode = {
      if (root == null) {
        return null
      }

      var head: DoublyListNode = null
      var prev: DoublyListNode = null

      def convert(node: TreeNode): Unit = {
        if (node != null) {
          convert(node.left)

          val current = new DoublyListNode(node.value)
          if (head == null) {
            head = current
          } else {
            prev.next = current
            current.prev = prev
          }
          prev = current

          convert(node.right)
        }
      }

      convert(root)

      // Connect the head and tail of the doubly linked list
      if (head != null) {
        head.prev = prev
        prev.next = head
      }

      head
    }

  def printDoublyList(head: DoublyListNode): Unit = {
    var current = head
    var cnt = 0
    while (current != null && cnt <1) {
      print(current.value + " ")
      current = current.next
      if (current.value == head.value) cnt += 1
    }
  }
      val root = new TreeNode(4)
      root.left = new TreeNode(2, new TreeNode(1), new TreeNode(3))
      root.right = new TreeNode(5)
      val result = treeToDoublyList(root)
      printDoublyList(result)
  }

