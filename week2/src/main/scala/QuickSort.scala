/**
  * Created by Dandoh on 9/13/16.
  */
object QuickSort {
  object PartitionStrategy extends Enumeration {
    type Strategy = Value
    val LEFT, RIGHT, MEDIAN = Value
  }

  import PartitionStrategy._

  var strategy = LEFT

  /**
    *
    * @param input Input array
    * @param left Left most index (inclusive)
    * @param right Right most index (inclusive)
    * @return The mid index after partition (left -> mid - 1 | mid | mid + 1 -> right)
    */
  def partition(input: Array[Int], left: Int, right: Int): Int = {

    def swap(index1: Int, index2: Int): Unit = {
      val temp = input(index1)
      input(index1) = input(index2)
      input(index2) = temp
    }


    val pivotIndex = strategy match {
      case LEFT => left
      case RIGHT => right
      case MEDIAN =>
        val mid = left + (right - left) / 2
        if (isBetween(input(mid), input(left), input(right)))
          mid
        else if (isBetween(input(left), input(right), input(mid)))
          left
        else
          right
    }
    swap(left, pivotIndex)

    // make pivot the first element by swapping
    val pivot = input(left)
    var i = left + 1
    for (j <- left + 1 to right) {
      if (input(j) < pivot) {
        swap(i, j)
        i += 1
      }
    }
    swap(left, i - 1)

    i - 1
  }

  def sort(input: Array[Int], left: Int, right: Int): Int = {
    if (left < right) {
      val mid = partition(input, left, right)
//      println(left + " " + mid + " " + right)
      val comparisonPartition = right - left
      val comparisonLeft = sort(input, left, mid - 1)
      val comparisonRight = sort(input, mid + 1, right)

      comparisonLeft + comparisonPartition + comparisonRight
    } else {
      0
    }
  }

  /**
    * @param input The array to be sorted
    * @return Number of comparisons made to perform Quicksort
    */
  def sort(input: Array[Int]): Int = sort(input, 0, input.length - 1)

  private def isBetween(number: Int, endPoint1: Int, endPoint2: Int): Boolean = {
    number >= Math.min(endPoint1, endPoint2) && number <= Math.max(endPoint1, endPoint2)
  }

  def main(args: Array[String]) {
    val input = {
      for {
        line <- io.Source.fromFile("QuickSort.txt").getLines
      } yield line.toInt
    }.toArray

    for (strg <- PartitionStrategy.values) {
      println(strg)
      strategy = strg

      val array = input.clone()
      val comparison = sort(array)

      println(comparison)
    }
  }
}
