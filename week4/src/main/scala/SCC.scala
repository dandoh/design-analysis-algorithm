import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * Created by Dandoh on 9/27/16.
  */
object SCC {

  /**
    * @param V   number of vertices
    * @param adj array of adjacency lists
    */
  case class DirectedGraph(V: Int, adj: Array[ArrayBuffer[Int]]) {

    /**
      * @return Reverse graph by reverse the direction of all edges.
      */
    def reverse: DirectedGraph = {
      val adjReverse: Array[ArrayBuffer[Int]] = Array.fill(V + 1)(ArrayBuffer())

      for {
        u <- 1 to V
        v <- adj(u)
      } {
        adjReverse(v) += u
      }

      DirectedGraph(V, adjReverse)
    }

  }

  /**
    * Read from file and build directed graph data structure
    *
    * @param V        Number of vertices
    * @param filename Graph representing file's name in which the i-th line have the structure "u v" indicate there is an edge u -> v
    * @return Directed graph
    */
  def buildGraph(V: Int, filename: String): DirectedGraph = {
    val adj: Array[ArrayBuffer[Int]] = Array.fill(V + 1)(ArrayBuffer())

    for {
      line <- Source.fromFile(filename).getLines()
      vertices = line.split("\\s+").map(_.toInt)
      (u, v) = (vertices(0), vertices(1))
    } {
      adj(u) += v
    }

    DirectedGraph(V, adj)
  }

  /**
    * Compute the order by which the second pass of DFS use iterate. First reverse the Graph, then use DFS
    * to compute the finish-time of each vertex.
    *
    * @param G Directed Graph
    * @return Order of vertices sorted by finished time
    */
  def computeOrder(G: DirectedGraph): Array[Int] = {
    // finish-time
    val ft: Array[Int] = new Array(G.V + 1)
    val marked: Array[Boolean] = Array.fill(G.V + 1)(false)
    // current finishing time
    var currentFt = 0

    def dfs(G: DirectedGraph, s: Int): Unit = {
      marked(s) = true
      for (v <- G.adj(s)) {
        if (!marked(v)) {
          dfs(G, v)
        }
      }
      // increase and set finishing time
      currentFt += 1
      ft(currentFt) = s
    }

    val Greverse = G.reverse
    for (i <- 1 to Greverse.V) {
      if (!marked(i))
        dfs(Greverse, i)
    }

    ft
  }

  /**
    * Compute SCCs of given directed graph
    *
    * @param G Directed Graph
    * @return Array of leader of each vertex
    */
  def computeSCC(G: DirectedGraph): Array[Int] = {
    // compute the "magic" order
    val order = computeOrder(G)
    val marked = Array.fill(G.V + 1)(false)
    val leader = Array.fill(G.V + 1)(0)

    // current leader using in DFS loop
    var currentLeader = 0

    def dfs(G: DirectedGraph, s: Int): Unit = {
      marked(s) = true

      for (v <- G.adj(s)) {
        if (!marked(v)) {
          dfs(G, v)
        }
      }

      leader(s) = currentLeader
    }

    // DFS loop
    for {
      // from V down to 1
      i <- G.V to 1 by -1
      u = order(i)
      if !marked(u)
    } {
      currentLeader = u
      dfs(G, u)
    }

    leader
  }

  /**
    * Return sizes of n largest SCCs in non-decreasing order
    * @param leader array of leaders compute in Kosariju's algorithm
    * @param n number of SCCs
    * @return
    */
  def topSCC(leader: Array[Int], n: Int): Array[Int] = {
    val res = new Array(n)
    // bin array
    val bin = Array.fill(leader.length)(0)

    for (ld <- leader) {
      bin(ld) += 1
    }
    bin.tail.sortWith(_ > _).take(n)
  }

  def main(args: Array[String]): Unit = {


  }


}
