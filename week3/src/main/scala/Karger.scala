import scala.io.Source

/**
  * Created by Dandoh on 9/21/16.
  */
object Karger {

  /**
    * Union Find data structure, nodes are numbered from 1 to n (inclusively)
    */
  class UF(n: Int) {
    // parent of each node, initial value is itself
    val parent: Array[Int] = new Array[Int](n + 1).zipWithIndex.map { case (_, index) => index }

    // rank of each node, with initial value 1
    val rank: Array[Int] = Array.fill(n + 1)(1)

    /**
      * Union two given nodes
      *
      * @param u first node
      * @param v second node
      */
    def union(u: Int, v: Int): Unit = {
      if (u > n || u <= 0) throw new IndexOutOfBoundsException
      if (v > n || v <= 0) throw new IndexOutOfBoundsException

      val ru = root(u)
      val rv = root(v)

      if (rank(ru) >= rank(rv)) {
        parent(rv) = ru
        rank(ru) = Math.max(rank(ru), rank(rv) + 1)
      } else {
        parent(rv) = ru
        rank(rv) = Math.max(rank(rv), rank(ru) + 1)
      }
    }

    /**
      * Check if two node are joined
      *
      * @param u first node
      * @param v second node
      * @return true if two node is in the same set, false otherwise.
      */
    def find(u: Int, v: Int): Boolean = {
      if (u > n || u <= 0) throw new IndexOutOfBoundsException
      if (v > n || v <= 0) throw new IndexOutOfBoundsException

      root(u) == root(v)
    }

    private def root(u: Int): Int = {
      if (u > n || u <= 0) throw new IndexOutOfBoundsException

      var i = u
      while (parent(i) != i) {
        i = parent(i)
      }
      i
    }
  }

  case class Graph(V: Int, E: Int, edges: Array[(Int, Int)]) {
    override def toString = {
      edges.mkString(", ")
    }
  }


  def minimumCut(G: Graph): Int = {
    def iterate(): Int = {
      val uf = new UF(G.V)

      // Karger's algorithm iterate
      var numVertices = G.V
      // shuffle the edges list
      val edges = util.Random.shuffle(G.edges.toList)
      for ((u, v) <- edges) {
        if (numVertices > 2)
          // if two vertices is not joined together
          if (!uf.find(u, v)) {
            uf.union(u, v)
            numVertices -= 1
          }
      }

      edges.foldLeft(0) {
        case (acc, (u, v)) =>
          if (!uf.find(u, v)) {
            acc + 1
          }
          else acc
      }
    }

    // number of interation
    val numIterate = G.V * G.V
    println("Number of iteration " + numIterate)

    // run the algorithm numIterate times and determine the minimum cut
    (1 to numIterate).foldLeft(Int.MaxValue)((min, _) => {
//      println(s"iteration number : $iter")
      val res = iterate()
      if (res < min) res
      else min
    })

  }

  def buildGraph(filename: String): Graph = {
    val adjacencyLists = (for {
      line <- Source.fromFile(filename).getLines()
    } yield line).toArray

    val V = adjacencyLists.length

    // construct set of edges, no parallel edges
    val setEdges = (for {
      line <- adjacencyLists
      vertices = line.split("\\s+").map(_.toInt)
      u = vertices.head
      v <- vertices.tail
    } yield {
      if (u < v) (u, v)
      else (v, u)
    }).toSet

    Graph(V, setEdges.size, setEdges.toArray)
  }


  def main(args: Array[String]) {
    val res = minimumCut(buildGraph("Karger.txt"))
    println(res)
  }


}