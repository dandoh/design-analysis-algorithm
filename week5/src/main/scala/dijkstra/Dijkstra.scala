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

    shortestPath
  }

  def main(args: Array[String]): Unit = {
    val G = buildGraph("dijkstraData.txt", 200)
    val res = compute(G, 1)
    println(res(1))
    val list = List(7,37,59,82,99,115,133,165,188,197)
    val p = res.zipWithIndex.filter{case (_, id) => list.contains(id)}.map {case (r, _) => r}
    println(p mkString ",")

  }


}
