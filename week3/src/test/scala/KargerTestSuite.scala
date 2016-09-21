import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.util.Random

/**
  * Created by Dandoh on 9/21/16.
  */
@RunWith(classOf[JUnitRunner])
class KargerTestSuite extends FunSuite{
  import Karger._
  test("Union find") {
    val uf = new UF(5)
    uf.union(1, 2)
    uf.union(2, 5)
    uf.union(3, 4)

    assert(uf.find(1, 5))
    assert(!uf.find(2, 3))
  }

  test("Union find 2") {
    val N = 100
    val uf = new UF(N)

    for (i <- 1 to 99) {
      uf.union(i, i + 1)
    }

    for (i <- 1 to 213) {
      assert(uf.find(Random.nextInt(N) + 1, Random.nextInt(N) + 1))
    }

  }

  test("Build graph") {
    val G = buildGraph("test1.txt")

    assert(G.V === 8)
    assert(G.E === 14)
  }

  trait TestSet1 {
    val filename = "test1.txt"
  }

  trait TestSet2 {
    val filename = "test2.txt"
  }

  trait TestSet3 {
    val filename = "test3.txt"
  }

  trait TestSet4 {
    val filename = "test4.txt"
  }

  test("test 1") {
    new TestSet1 {
      assert(minimumCut(buildGraph(filename)) === 2)
    }
  }

  test("test 2") {
    new TestSet2 {
      assert(minimumCut(buildGraph(filename)) === 2)
    }
  }

  test("test 3") {
    new TestSet3 {
      assert(minimumCut(buildGraph(filename)) === 1)
    }
  }

  test("test 4") {
    new TestSet4 {
      assert(minimumCut(buildGraph(filename)) === 3)
    }
  }
}
