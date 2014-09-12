package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.reflect.internal.util.StringOps

class FizzBuzzSpec extends Specification with ThrownExpectations {

  val digitIsDigit: Int => String = input => input.toString

  val threesIsFizz: Int => Option[String] = input => if (input % 3 == 0) Some("fizz") else None

  val fiveIsBuzz: Int => Option[String] = input => if (input % 5 == 0) Some("buzz") else None

  val solutionStrategies = Seq(threesIsFizz, fiveIsBuzz)

  def fizzBuzzSolution(input: Int): String = {
    solutionStrategies.map(_(input)).map(_.getOrElse("")).mkString
  }

  def fizzbuzz(input: Int): String = {
    if (fizzBuzzSolution(input).nonEmpty) fizzBuzzSolution(input) else digitIsDigit(input)
  }

  "fizz buzz" should {
    Seq(1 -> "1",
      2 -> "2",
      3 -> "fizz",
      4 -> "4",
      5 -> "buzz",
      6 -> "fizz",
      7 -> "7",
      8 -> "8",
      9 -> "fizz",
      15 -> "fizzbuzz") foreach { case (input, expected) =>
      s"return $expected for $input" in {

        fizzbuzz(input) must beEqualTo(expected)
      }
    }
  }


}
