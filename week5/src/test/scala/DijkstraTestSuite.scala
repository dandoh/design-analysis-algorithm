import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by Dandoh on 10/4/16.
  */
@RunWith(classOf[JUnitRunner])
class DijkstraTestSuite extends FunSuite{
  import dijkstra._

  test("test 1") {
    val G = graph.buildGraph("test.txt", 8)

    assert(Dijkstra.compute(G, 1) === Array(0, 1, 2, 3, 4, 4, 3, 2))
  }
}
