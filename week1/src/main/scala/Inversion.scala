/**
  * Created by Dandoh on 9/6/16.
  */
object Inversion {

  def main(args: Array[String]): Unit = {

    val input = {
      for {
        line <- io.Source.fromFile("IntegerArray.txt").getLines
      } yield line.toInt
    }.toArray

    println(sortAndCountInversion(input, 0, input.length))


    println(input.mkString(", "))
//    println(input.length)
  }

  val threshold = 4

  /**
    *
    * @param inp   input array
    * @param start start index (inclusive)
    * @param end   end index (exclusive)
    */
  def sortAndCountInversion(inp: Array[Int], start: Int, end: Int): Long = {
    if (end - start < threshold) {
      val res = countInversion(inp, start, end)
      insertionSort(inp, start, end)
      res
    } else {
      val mid = (start + end) / 2

      val numInversionLeft = sortAndCountInversion(inp, start, mid)
      val numInversionRight = sortAndCountInversion(inp, mid, end)
      val numSplitInversion = mergeAndCountSplitInversion(inp, start, mid, end)

      numInversionLeft + numInversionRight + numSplitInversion
    }
  }

  def mergeAndCountSplitInversion(inp: Array[Int], start: Int, mid: Int, end: Int): Long = {
    val length = end - start
    val aux = new Array[Int](end - start)
    var numInversion = 0L

    // copy from inp to aux
    var i = start
    var j = mid
    for (k <- 0 until length) {
      if (i >= mid) { aux(k) = inp(j); j += 1 }
      else if (j >= end) { aux(k) = inp(i); i += 1}
      else {
        if (inp(j) < inp(i)) {
          aux(k) = inp(j)
          j += 1
          // TODO
          numInversion += (mid - i)
        } else {
          aux(k) = inp(i)
          i += 1
        }
      }
    }

    for (k <- 0 until length) {
      inp(start + k) = aux(k)
    }

    numInversion
  }

  def insertionSort(inp: Array[Int], start: Int, end: Int): Unit = {
    for (i <- start until end) {
      var j = i
      while (j - 1 >= start && inp(j - 1) > inp(j)) {
        swap(j, j - 1)
        j -= 1
      }
    }

    def swap(i: Int, j: Int): Unit = {
      val temp = inp(i)
      inp(i) = inp(j)
      inp(j) = temp
    }
  }

  def countInversion(inp: Array[Int], start: Int, end: Int): Long = {
    {
      for {
        i <- start + 1 until end
        j <- start until i // j < i
        if inp(j) > inp(i)
      } yield 1L
    }.sum
  }


}
