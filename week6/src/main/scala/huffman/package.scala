/**
  * Created by Dandoh on 3/5/17.
  */
package object huffman {

  abstract class CodeTree {
    def getWeight = this match {
      case Fork(_, _, _, weight) => weight
      case Leaf(_, weight) => weight
    }

    def getSymbols = this match {
      case Fork(_, _, symbols, _) => symbols
      case Leaf(symbol, _) => List(symbol)
    }
  }

  case class Fork(left: CodeTree, Right: CodeTree, symbols: List[Int], weight: Long) extends CodeTree

  case class Leaf(symbol: Int, weight: Long) extends CodeTree

  def minimumDepth(tree: CodeTree): Int = tree match {
    case Leaf(_, _) => 0
    case Fork(left, right, _, _) =>
      Math.min(minimumDepth(left), minimumDepth(right)) + 1
  }

  def maximumDepth(tree: CodeTree): Int = tree match {
    case Leaf(_, _) => 0
    case Fork(left, right, _, _) =>
      Math.max(maximumDepth(left), maximumDepth(right)) + 1
  }

  // encode
  def encode(symbolAndWeight: List[(Long, Int)]): CodeTree = {
    val leaves = symbolAndWeight.map { case (weight, symbol) => Leaf(symbol, weight) }

    def encodeAcc(trees: List[CodeTree]): CodeTree = {
      if (trees.length == 1) trees.head
      else {
        encodeAcc(mergeTwoSmallest(trees))
      }
    }

    encodeAcc(leaves)
  }

  def mergeTwoSmallest(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil => Nil
    case x :: Nil => trees
    case x :: y :: tail =>
      val sortedByWeight = trees.sortWith { case (tree1, tree2) => tree1.getWeight < tree2.getWeight }
      sortedByWeight match {
        case first :: second :: rest =>
          val merged = Fork(first, second, first.getSymbols ++ second.getSymbols, first.getWeight + second.getWeight)
          merged :: rest
      }
  }


  def main(args: Array[String]) {
    val input = {
      for {
        line <- io.Source.fromFile("huffman.txt").getLines
      } yield line.toLong
    }.toList.zipWithIndex

    val encodedTree = encode(input)

    println(maximumDepth(encodedTree))
    println(minimumDepth(encodedTree))

  }


}
