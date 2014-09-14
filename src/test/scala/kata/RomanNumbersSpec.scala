package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.reflect.internal.util.StringOps

class RomanNumbersSpec extends Specification with ThrownExpectations {

  case class RomanCharacters(one: String, five: String, ten: String)

  object one extends RomanCharacters("I", "V", "X")

  object ten extends RomanCharacters("X", "L", "C")

  object hundred extends RomanCharacters("C", "D", "M")

  object thousand extends RomanCharacters("M", "", "")

  val romanCharacters = Seq(one,ten,hundred,thousand)

  def toRoman(value: Int): String = {
    value.toString.reverse.zipWithIndex.map(charToRoman).reverse.mkString
  }

  def charToRoman(a: (Char, Int)) = digitToRoman(a._1.toString.toInt, romanCharacters(a._2))

  def digitToRoman(digit: Int, characters: RomanCharacters): String = digit match {
    case 4 => characters.one + characters.five
    case 9 => characters.one + characters.ten
    case digit if digit >= 5 => characters.five + digitToRoman(digit - 5, characters)
    case _ => Seq.fill(digit)(characters.one).mkString
  }

  "converter" should {
    Seq(
      1 -> "I",
      2 -> "II",
      3 -> "III",
      4 -> "IV",
      5 -> "V",
      6 -> "VI",
      7 -> "VII",
      8 -> "VIII",
      9 -> "IX",
      10 -> "X",
      14 -> "XIV",
      40 -> "XL",
      48 -> "XLVIII",
      50 -> "L",
      500 -> "D",
      999 -> "CMXCIX",
      1000 -> "M",
      1904 -> "MCMIV"
    ) foreach { case (input, expected) =>
      s"convert $input to $expected" in {

        toRoman(input) must beEqualTo(expected)
      }
    }
  }


}
