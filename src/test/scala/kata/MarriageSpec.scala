package kata

import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations

import scala.annotation.tailrec


object PersonImplicits {

  implicit class Person(val name: String) extends AnyVal {

    override def toString = name
  }

}

/**
 * This is a slight variation over the original stable marriage problem
 * rosettacode.org/wiki/Stable_marriage_problem
 *
 * my solution uses a simplified version of 'best match' such as that the sum of mispreferences is optimized to be the smallest.
 *
 */

class MarriageSpec extends Specification with AllExpectations {

  import kata.PersonImplicits._

  def lazyMinBy[A](seq: Iterator[Seq[A]], score: A => Int): Seq[A] = {
    @tailrec
    def minIn(solution: (Int, Seq[A]), seq: Seq[Seq[A]]): (Int, Seq[A]) = {
      seq match {
        case Nil => solution
        case head +: tail => {
          val growingSums: Stream[Int] = head.toStream.map(score).scanLeft(0)(_ + _)
          if (growingSums.find(_ > solution._1) == None) minIn((growingSums.last, head), tail)
          else minIn(solution, tail)
        }
      }
    }
    minIn((Int.MaxValue, Nil), seq.toStream)._2
  }

  def dating(males: Seq[Person], females: Seq[Person], preference: Person => Seq[Person]): Person => Person = {
    val pairScore: ((Person, Person)) => Int = {
      case (men, female) => preference(men).indexOf(female) + preference(female).indexOf(men)
    }

    val all: Iterator[Seq[(Person, Person)]] = males.permutations.map(_.zip(females))
    val best: Seq[(Person, Person)] = lazyMinBy(all, pairScore)
    val bestFull: Seq[(Person, Person)] = best.flatMap { pair => Seq(pair, pair.swap)}
    bestFull.toMap
  }

  "dating service" should {
    "find ideal marriages" in {
      val preference: (Person) => Person = dating(Seq("jon", "ken"), Seq("kate", "mary"),
        Map(new Person("jon") -> Seq("kate", "mary"),
          new Person("ken") -> Seq("mary", "kate"),
          new Person("kate") -> Seq("jon", "ken"),
          new Person("mary") -> Seq("ken", "jon")))

      preference("jon") must beEqualTo[Person]("kate")
      preference("ken") must beEqualTo[Person]("mary")
      preference("kate") must beEqualTo[Person]("jon")
      preference("mary") must beEqualTo[Person]("ken")
    }

    def persons(s: String) = s.trim.split(",").map(_.trim).map(new Person(_)).toSeq
    def preferences(s: String) = s.split("\\n").map(_.trim).map(line => new Person(line.split(":")(0)) -> persons(line.split(":")(1))).toSeq.toMap

    "find ideal marriages 2" in {
      val males = persons("abe, bob, col, dan, ed, fred, gav, hal, ian, jon")
      val females = persons("abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan")

      val malePreferences = preferences( """abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay
      bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay
      col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan
      dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi
      ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay
      fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay
      gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay
      hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee
      ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve
      jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope""")

      val femalePreferences = preferences( """abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal
      bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal
      cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon
      dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed
      eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob
      fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal
      gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian
      hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred
      ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan
      jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan""")

      val preference: (Person) => Person = dating(males, females, malePreferences ++ femalePreferences)

      preference("dan") must beEqualTo[Person]("fay")
      preference("col") must beEqualTo[Person]("dee")
      preference("hal") must beEqualTo[Person]("eve")
      preference("gav") must beEqualTo[Person]("gay")
      preference("fred") must beEqualTo[Person]("bea")
      preference("ed") must beEqualTo[Person]("jan")
      preference("abe") must beEqualTo[Person]("ivy")
      preference("ian") must beEqualTo[Person]("hope")
      preference("bob") must beEqualTo[Person]("cath")
      preference("jon") must beEqualTo[Person]("abi")
    }
  }

  "minFunction" should {
    "handle infinite streams" in {
        lazyMinBy[Int](Seq(Seq(1,2,3),Seq(1,3,3),Seq(10,10,10)).iterator,a => a) must beEqualTo(Seq(1,2,3))
        lazyMinBy[Int](Seq(Seq(1,2,3),Stream.iterate(1)(_ + 1)).iterator,a => a) must beEqualTo(Seq(1,2,3))
    }
  }


}
