package kata

import org.specs2.mutable.Specification

/**
 * https://www.hackerrank.com/challenges/john-and-fences
 */
class FencesSpec extends Specification {

  def rectangleSizeIn(ints: Seq[Int]) = {
    ints.sliding(ints.size).map(fenceHeights => fenceHeights.tail.takeWhile(height => height >= fenceHeights.head).size * fenceHeights.head).max
  }

  "john" should {
    "find the biggest rectangle in a fence" in {
      rectangleSizeIn("2 5 7 4 1 8".split(" ").toSeq.map(_.toInt)) must beEqualTo(12)
    }
  }

}
