package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.util.Try

class CodeKata4Spec extends Specification with ThrownExpectations {

  trait FormatParser {

    def getInt(stringValue: String): Int = Try(stringValue.toInt).getOrElse(stringValue.replaceFirst("\\*", "").toInt)

    def minimumSpread(fileName: String): String = {
      val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(fileName)).getLines()

      input.map(parse(_)).filter(_.isDefined).minBy(_.get.spread).get.solution
    }

    def parse(value: String): Option[SolutionWithSpread]


  }

  class SolutionWithSpread(val solution: String, val spread: Int)

  "weather" should {

    case class DayEntry(day: String, max: Int, min: Int) extends SolutionWithSpread(day,max-min)

    class WeatherFormatParser extends FormatParser {

      def parse(value: String): Option[DayEntry] = {
        val split: Array[String] = value.split("\\s+")
        val dayNumber: String = split.lift(1).getOrElse("")
        Try(dayNumber.toInt).map(_ => DayEntry(dayNumber, getInt(split(2)), getInt(split(3)))).toOption
      }

    }

    "calculate day with minimum temperature spread" in {

      new WeatherFormatParser().minimumSpread("weather.dat") must beEqualTo("1")
    }
  }
  "football" should {
    case class FootballTeam(name: String, scored: Int, lost: Int) extends SolutionWithSpread(name,Math.abs(scored - lost))

    class FootballFormatParser extends FormatParser {

      def parse(value: String): Option[FootballTeam] = {
        val split: Array[String] = value.split("\\s+")
        val index: String = split.lift(1).getOrElse("")
        Try(index.replaceFirst("\\.", "").toInt).map(_ => FootballTeam(split(2), getInt(split(7)), getInt(split(9)))).toOption
      }
    }

    "find club with smallest goal difference" in {
      new FootballFormatParser() minimumSpread ("football.dat") must beEqualTo("Aston_Villa")
    }
  }


}
