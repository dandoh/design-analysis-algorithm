package dijkstra

import scala.reflect.ClassTag

/**
  * Min priority queue of given n items in which the positions of items in PQ are stored.
  *
  * @param n number of items
  * @tparam A key type
  */
class IndexedMinPQ[A](n: Int, lessThan: (A, A) => Boolean)(implicit m: ClassTag[A]) {

  // position of item in the queue
  val position: Array[Int] = Array.fill(n + 1)(-1)

  // the key of each items, i.e key[1] is the key of item 1
  val key = new Array[A](n + 1)

  // min pq of items
  val pq: Array[Int] = Array.fill(n + 1)(0)

  var size = 0

  // Indicate whether PQ contains i-th item or not
  def contain(i: Int): Boolean = {
    position(i) != -1
  }

  /**
    * Insert ith item to pq with given key
    *
    * @param i
    */
  def insert(i: Int, keyi: A): Unit = {
    key(i) = keyi
    size += 1
    pq(size) = i
    position(i) = size

    swim(size)
  }

  /**
    * @return The item with minimum key
    */
  def extractMin: Int = {
    // item in the first position (root)
    val res = pq(1)
    // swap with the last item
    swap(1, size)
    // last item is no longer in PQ
    position(pq(size)) = -1
    size -= 1
    // maintain heap property
    sink(1)

    res
  }

  /**
    * Delete ith item from pq
    */
  def delete(i: Int): Unit = {
    if (contain(i)) {
      val oldPosition = position(i)
      swap(position(i), size)
      // last item is no longer in PQ
      position(pq(size)) = -1
      size -= 1

      sink(oldPosition)
      swim(oldPosition)
    }
  }

  def isEmpty: Boolean = size == 0

  private def swim(position: Int): Unit = {
    var k = position
    while (k > 1 && lessThan(key(pq(k)), key(pq(k / 2)))) {
      swap(k, k / 2)
      k = k / 2
    }
  }

  private def sink(position: Int): Unit = {
    var minPosition = position
    if (2 * position <= size &&
      lessThan(key(pq(2 * position)), key(pq(minPosition)))) {
      minPosition = 2 * position
    }
    if (2 * position + 1 <= size &&
      lessThan(key(pq(2 * position + 1)), key(pq(minPosition)))) {
      minPosition = 2 * position + 1
    }

    if (position != minPosition) {
      swap(position, minPosition)
      sink(minPosition)
    }
  }

  private def swap(position1: Int, position2: Int): Unit = {
    // for example :  pq(1) = 3, pq(2) = 4
    //                position(3) = 1, position(4) = 2
    // after swap :   pq(1) = 4, pq(2) = 3
    //                position(4) = 1, position(3) = 2

    // keep tract of the position
    position(pq(position1)) = position2
    position(pq(position2)) = position1

    // swap item
    val temp = pq(position1)
    pq(position1) = pq(position2)
    pq(position2) = temp
  }

  override def toString = {
    pq.tail.take(size).mkString(", ") + "\nPosition: " + position.tail.mkString(", ") + "\n"
  }
}
