import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


/**
  * Created by Dandoh on 9/6/16.
  */

@RunWith(classOf[JUnitRunner])
class InversionTestSuite extends FunSuite {

  import Inversion._

  trait TestSet {
    val inp = Array(3, 5, 1, 2, 4)
  }

  val LENGTH = 100;

  trait TestSet2 {
    val inp = util.Random.shuffle(1 to LENGTH).toArray
  }

  trait TestSet3 {
    val inp = (1 to 1000).reverse.toArray
  }

  test("Reverse Order") {
    new TestSet3 {
      assert(sortAndCountInversion(inp, 0, inp.length) === 1000 * 999 / 2)

      val inp2 = (1 to 1000).toArray
      assert(sortAndCountInversion(inp2, 0, inp2.length) === 0)
    }
  }

  test("Sorted property") {
    new TestSet2 {
      sortAndCountInversion(inp, 0, inp.length)

      assert(inp === (1 to LENGTH).toArray)
    }
  }

  test("Insertion sort") {
    new TestSet {
      insertionSort(inp, 0, inp.length)
      assert(inp sameElements Array(1, 2, 3, 4, 5))
    }

    new TestSet2 {
      insertionSort(inp, 0, inp.length)

      assert(inp === (1 to LENGTH).toArray)
    }
  }

  test("Count inversion") {
    new TestSet {
      assert(countInversion(inp, 0, inp.length) === 5)
    }
  }


  test("Count inversion devide and conquer") {
    new TestSet {
      assert(sortAndCountInversion(inp, 0, inp.length) === 5)
    }
  }

  test("Count inversion devide and conquer 2") {
    new TestSet2 {
      val inp2 = inp.clone()
      assert(sortAndCountInversion(inp, 0, inp.length) === countInversion(inp2, 0, inp2.length))
    }
  }


}
