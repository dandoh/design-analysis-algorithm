import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by Dandoh on 10/4/16.
  */
package object graph {

  case class UndirectedWeightedGraph(V: Int, adjs: Array[ArrayBuffer[WeightedEdge]]) {
    override def toString = {
      {
        for {
          (adj, vertex) <- adjs.zipWithIndex.tail
        } yield vertex + ": " +
          adj.map{case WeightedEdge(u, v, weighted) => (v, weighted)}.mkString(", ")
      }.mkString("\n")
    }
  }

  case class WeightedEdge(u: Int, v: Int, weight: Int) {
    /**
      * Return vertex other than a given vertex of this edge.
      * @param x one of two endpoint
      */
    def other(x: Int): Int = {
      if (x != u && x != v) throw new IllegalArgumentException

      if (x == u) v
      else v
    }
  }

  /**
    * Build a Undirected Weighted Graph given file name
    */
  def buildGraph(filename: String, V: Int): UndirectedWeightedGraph = {
    val adj: Array[ArrayBuffer[WeightedEdge]] = Array.fill(V + 1)(ArrayBuffer())

    for {
      line <- Source.fromFile(filename).getLines()
      raw = line.split("\\s+")
      u = raw.head.toInt
      edge <- raw.tail.map{r =>
        val xs = r.split(",").map(_.toInt)
        WeightedEdge(u, xs(0), xs(1))
      }
    } {
      adj(u) += edge
    }

    UndirectedWeightedGraph(V, adj)
  }





}
