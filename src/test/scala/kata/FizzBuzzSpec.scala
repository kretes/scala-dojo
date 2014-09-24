package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.annotation.tailrec
import scala.reflect.internal.util.StringOps

class FizzBuzzSpec extends Specification with ThrownExpectations {

  object FizzBuzzWithStrategies {

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
  }

  object FizzBuzzWithPatternMatch {

    implicit def valToSome[T](a: T) = Option(a)

    @tailrec
    def divideFinally(value: Int, divisor: Int): Int = if (value % divisor == 0) divideFinally(value / divisor, divisor) else value

    def fizzOrBuzz(input: Int): Option[String] = input match {
      case _ if input % 3 == 0 => "fizz" + fizzOrBuzz(divideFinally(input, 3)).getOrElse("")
      case _ if input % 5 == 0 => "buzz" + fizzOrBuzz(divideFinally(input, 5)).getOrElse("")
      case _ => None
    }

    def fizzbuzz(input: Int): String = fizzOrBuzz(input).getOrElse(input.toString)

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

        FizzBuzzWithStrategies.fizzbuzz(input) must beEqualTo(expected)
        FizzBuzzWithPatternMatch.fizzbuzz(input) must beEqualTo(expected)
      }
    }
  }


}
