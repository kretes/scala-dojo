package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.util.Try

class CodeKata6AnagramsSpec extends Specification with ThrownExpectations {

  def anagramsIn(fileName: String) : Set[Set[String]] = {
    val input = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(fileName)).getLines().toSeq

    input.groupBy(_.sorted).values.filter(_.size > 1).map(_.toSet).toSet
  }

  "anagram finder" should {
    "find all anagrams in short list" in {
      anagramsIn("shortWordList.txt") must containAllOf(
        Seq(
          Set("kinship", "pinkish"),
          Set("enlist", "inlets", "listen", "silent"),
          Set("boaster", "boaters", "borates"),
          Set("fresher", "refresh"),
          Set("sinks", "skins"),
          Set("knits", "stink"),
          Set("rots", "sort")
        )
      )
    }

    "find all anagrams in short list" in {
      val allAnagrams: Set[Set[String]] = anagramsIn("wordlist.txt")

      allAnagrams.size must beEqualTo(20683)
      allAnagrams must containAllOf(
        Seq(
            Set("crepitus", "cuprites", "pictures", "piecrust"),
            Set("paste", "pates", "peats", "septa", "spate", "tapes", "tepas"),
            Set("punctilio", "unpolitic"),
            Set("sunders", "undress")
        )
      )
    }
  }


}
