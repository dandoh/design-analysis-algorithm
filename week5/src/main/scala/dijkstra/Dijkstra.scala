package dijkstra

import graph._

/**
  * Created by Dandoh on 10/4/16.
  */
object Dijkstra {

  /**
    * Compute the shortest path of undirected graph using Dijkstra algorithm
    * @param G Undirected Graph
    * @param s Source vertex
    * @return Array of shortest path of all vertices (from 1 to G.V)
    */
  def compute(G: UndirectedWeightedGraph, s: Int): Array[Int] = {
    val pq = new IndexedMinPQ[Int](G.V, _ < _)


    // initial value
    val shortestPath: Array[Int] = Array.fill(G.V + 1)(Int.MaxValue)
    shortestPath(s) = 0


    pq.insert(s, 0)

    // Dijkstra's algorithm
    while (!pq.isEmpty) {
      // Choose the one with minimum distance to source
      val u = pq.extractMin
      for (e <- G.adjs(u)) {
        val v = e.other(u)
        if (shortestPath(v) > shortestPath(u) + e.weight) {
          shortestPath(v) = shortestPath(u) + e.weight
          if (!pq.contain(v)) {
            pq.insert(v, shortestPath(v))
          } else {
            pq.delete(v)
            pq.insert(v, shortestPath(v))
          }
        }
      }
    }

    shortestPath.tail
  }

  def main(args: Array[String]) {
  }


}
