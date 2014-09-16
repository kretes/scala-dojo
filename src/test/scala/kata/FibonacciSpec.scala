package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

class FibonacciSpec extends Specification with ThrownExpectations {

  case class FibonacciNumber(previous: Long, current: Long) {
    lazy val next = FibonacciNumber(current, previous + current)
  }

  object One extends FibonacciNumber(0, 1)

  lazy val fibonacci: Stream[Long] = Stream.iterate[FibonacciNumber](One)(number => number.next).map(_.current)

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

        fibonacci(input - 1) must beEqualTo(expected)
      }
    }
  }


}
