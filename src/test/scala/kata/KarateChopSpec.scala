package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

class KarateChopSpec extends Specification with ThrownExpectations {

  def chop(i: Int, ints: Seq[Int]) = ints.indexOf(i)

  def assert_equal(expected: Int, actual: Int) = {
    expected should be_==(actual)
  }

  "karate" should {
    "chop" in {

      assert_equal(-1, chop(3, Seq()))
      assert_equal(-1, chop(3, Seq(1)))
      assert_equal(0, chop(1, Seq(1)))
      assert_equal(0, chop(1, Seq(1, 3, 5)))
      assert_equal(1, chop(3, Seq(1, 3, 5)))
      assert_equal(2, chop(5, Seq(1, 3, 5)))
      assert_equal(-1, chop(0, Seq(1, 3, 5)))
      assert_equal(-1, chop(2, Seq(1, 3, 5)))
      assert_equal(-1, chop(4, Seq(1, 3, 5)))
      assert_equal(-1, chop(6, Seq(1, 3, 5)))
      assert_equal(0, chop(1, Seq(1, 3, 5, 7)))
      assert_equal(1, chop(3, Seq(1, 3, 5, 7)))
      assert_equal(2, chop(5, Seq(1, 3, 5, 7)))
      assert_equal(3, chop(7, Seq(1, 3, 5, 7)))
      assert_equal(-1, chop(0, Seq(1, 3, 5, 7)))
      assert_equal(-1, chop(2, Seq(1, 3, 5, 7)))
      assert_equal(-1, chop(4, Seq(1, 3, 5, 7)))
      assert_equal(-1, chop(6, Seq(1, 3, 5, 7)))
      assert_equal(-1, chop(8, Seq(1, 3, 5, 7)))
    }
  }


}
