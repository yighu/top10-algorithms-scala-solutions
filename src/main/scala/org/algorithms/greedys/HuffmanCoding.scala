package org.algorithms.greedys
import scala.collection.mutable.PriorityQueue

case class Node(char: Char, frequency: Int, left: Option[Node] = None, right: Option[Node] = None)

object HuffmanCoding extends App{

  def buildHuffmanTree(input: String): Node = {
    val charFrequency = input.groupBy(identity).mapValues(_.length)
    val priorityQueue = PriorityQueue.empty[Node](Ordering.by(-_.frequency))

    charFrequency.foreach { case (char, frequency) =>
      priorityQueue.enqueue(Node(char, frequency, None, None))
    }

    while (priorityQueue.size > 1) {
      val left = priorityQueue.dequeue()
      val right = priorityQueue.dequeue()

      val internalNode = Node('0', left.frequency + right.frequency, Some(left), Some(right))
      priorityQueue.enqueue(internalNode)
    }

    priorityQueue.dequeue()
  }

  def encodeHuffmanTree(root: Option[Node], code: String = ""): Map[Char, String] = {
    if (root.isEmpty) Map.empty
    else if (root.get.left.isEmpty && root.get.right.isEmpty) Map(root.get.char -> code)
    else encodeHuffmanTree(root.get.left, code + "0") ++ encodeHuffmanTree(root.get.right, code + "1")
  }

  def huffmanEncode(input: String): (String,Node) = {
    val root = buildHuffmanTree(input)
    val encodingMap = encodeHuffmanTree(Some(root))
    (input.map(encodingMap).mkString, root)
  }


  def decode(encodedString: String, root: Node): String = {
    def decodeHelper(currentNode: Node, remainingBits: List[Char], decodedString: String): String = {
      currentNode match {
        case Node(value, _, None, None) => // Leaf node
          if (remainingBits.isEmpty) decodedString + value
          else decodeHelper(root, remainingBits, decodedString + value)
        case Node(_, _, Some(left), Some(right)) =>
          if (remainingBits.head == '0') decodeHelper(left, remainingBits.tail, decodedString)
          else decodeHelper(right, remainingBits.tail, decodedString)
        case _ => decodedString // Should not happen for a valid Huffman tree
      }
    }

    decodeHelper(root, encodedString.toList, "")
  }


  val inputString = "hello world"
  val (encodedString, huffmanTree) = huffmanEncode(inputString)

  println(s"Original String: $inputString")
  println(s"Encoded String: $encodedString")
  val decodedString = decode(encodedString, huffmanTree)
  println(s"Decoded String: $decodedString")
}