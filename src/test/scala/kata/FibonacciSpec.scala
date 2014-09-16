package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

class FibonacciSpec extends Specification with ThrownExpectations {

  def createFibonacciStream(prevprev: Long, prev: Long): Stream[Long] = (prevprev + prev) #:: createFibonacciStream(prev, prevprev + prev)

  lazy val fibonacci: Stream[Long] = 1 #:: 1 #:: createFibonacciStream(1, 1)

  def fibonacciValue(index: Int) = fibonacci(index - 1)

  "fibbonacci" should {
    Seq(
      1 -> 1,
      2 -> 1,
      3 -> 2,
      4 -> 3,
      5 -> 5,
      6 -> 8,
      7 -> 13,
      46 -> 1836311903,
      92 -> 7540113804746346429L
    ) foreach { case (input, expected) =>
      s"return $expected for $input" in {

        fibonacciValue(input) must beEqualTo(expected)
      }
    }
  }


}
