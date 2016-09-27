
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Dandoh on 9/27/16.
  */
@RunWith(classOf[JUnitRunner])
class SCCTestSuite extends FunSuite {

  import SCC._

  test("Reverse graph") {
    val adj: Array[ArrayBuffer[Int]] = Array(
      ArrayBuffer(),
      ArrayBuffer(2, 3, 4),
      ArrayBuffer(4),
      ArrayBuffer(4),
      ArrayBuffer()
    )

    val G = DirectedGraph(4, adj)
    assert(
      G.reverse.adj === Array(
        ArrayBuffer(),
        ArrayBuffer(),
        ArrayBuffer(1),
        ArrayBuffer(1),
        ArrayBuffer(1, 2, 3)
      )

    )
  }

  trait TestSet1 {
    val G = buildGraph(4, "test1.txt")
    val res = topSCC(computeSCC(G), 5)
  }

  trait TestSet2 {
    val G = buildGraph(7, "test2.txt")
    val res = topSCC(computeSCC(G), 5)
  }

  trait TestSet3 {
    val G = buildGraph(9, "test3.txt")
    val res = topSCC(computeSCC(G), 5)
  }

  trait TestSet4 {
    val G = buildGraph(8, "test4.txt")
    val res = topSCC(computeSCC(G), 5)
  }

  trait TestSet5 {
    val G = buildGraph(8, "test5.txt")
    val res = topSCC(computeSCC(G), 5)
  }

  trait TestSet6 {
    val G = buildGraph(12, "test6.txt")
    val res = topSCC(computeSCC(G), 5)
  }

  test("build graph") {
    new TestSet1 {
      assert(G.adj === Array(
        ArrayBuffer(),
        ArrayBuffer(2, 3, 4),
        ArrayBuffer(4),
        ArrayBuffer(4),
        ArrayBuffer()
      ))
    }

  }

  test("compute order") {
    new TestSet1 {
      assert(computeOrder(G)(4) === 4)
    }
  }

  test("compute order 2") {
    new TestSet2 {
      val fn = computeOrder(G)(7)
      assert(fn === 5 || fn === 6 || fn === 7)

      assert(topSCC(computeSCC(G), 5) === Array(4, 3, 0, 0, 0))
    }
  }

  test("test 3") {
    new TestSet3 {
      assert(res === Array(3, 3, 3, 0, 0))
    }
  }

  test("test 4") {
    new TestSet4 {
      assert(res === Array(3, 3, 2, 0, 0))
    }
  }

  test("test 5") {
    new TestSet5 {
      assert(res === Array(3, 3, 1, 1, 0))
    }
  }

  test("test 6") {
    new TestSet6 {
      assert(res === Array(6, 3, 2, 1, 0))
    }
  }


}
