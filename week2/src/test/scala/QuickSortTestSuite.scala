import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * Created by Dandoh on 9/14/16.
  */
@RunWith(classOf[JUnitRunner])
class QuickSortTestSuite extends FunSuite {

  import QuickSort._

  trait TestSet1 {
    val array = util.Random.shuffle(1 to 1000).toArray
  }

  trait TestSet2 {
    val array = util.Random.shuffle(1 to 100).toArray
  }

  test("partition with left most pivot") {
    val array = Array(5, 1, 2, 3, 4, 6, 7, 8, 9, 10)
    val mid = partition(array, 0, array.length - 1)
    assert(array === Array(4, 1, 2, 3, 5, 6, 7, 8, 9, 10))
    assert(mid === 4)
  }

  test("quick sort must correctly sort the array 1") {
    new TestSet2 {
      val comparisons = sort(array)
      assert(array === (1 to 100).toArray)
    }

  }

  test("quick sort must correctly sort the array 2") {
    new TestSet1 {
      val comparisons = sort(array)
      assert(array === (1 to 1000).toArray)
    }

  }


}
