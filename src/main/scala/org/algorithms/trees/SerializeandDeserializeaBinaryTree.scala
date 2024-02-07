package org.algorithms.trees

object SerializeandDeserializeaBinaryTree extends App {
  /**
   * https://www.geeksforgeeks.org/serialize-deserialize-binary-tree/
   * Serialize and Deserialize a Binary Tree
   * Serialization is to store the tree in a file so that it can be later restored. The structure of the tree must be
   * maintained. Deserialization is reading the tree back from a file.
   * The following beautiful solution is from chat gpt.
   */
  // Definition of a binary tree node
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])


  // Serialize a binary tree to a string using pre-order traversal
  def serialize(root: Option[TreeNode]): String = {
    root match {
      case Some(node) =>
        s"${node.value},${serialize(node.left)},${serialize(node.right)}"
      case None =>
        "null"
    }
  }

  // Deserialize a string to a binary tree
  def deserialize(data: String): Option[TreeNode] = {
    val values = data.split(",")
    val queue = scala.collection.mutable.Queue[String](values: _*)

    def buildTree(): Option[TreeNode] = {
      val value = queue.dequeue()
      if (value == "null") {
        None
      } else {
        val nodeValue = value.toInt
        val left = buildTree()
        val right = buildTree()
        Some(TreeNode(nodeValue, left, right))
      }
    }

    buildTree()
  }

  val tree = TreeNode(1,
    Some(TreeNode(2, Some(TreeNode(4, None, None)), None)),
    Some(TreeNode(3, None, Some(TreeNode(5, None, None)))))
  val serializedTree = serialize(Some(tree))
  println(s"Serialized Tree: $serializedTree")

  val deserializedTree = deserialize(serializedTree)

  println(s"Deserialized Tree: ${deserializedTree.get}")
  println(s"Original     Tree: $tree")
}
