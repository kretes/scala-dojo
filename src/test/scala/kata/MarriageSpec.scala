package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification


object Implicits {

  implicit class Person(val name: String) extends AnyVal {

    override def toString = name
  }

}

class MarriageSpec extends Specification with ThrownExpectations {

  import kata.Implicits._


  def dating(males: Seq[Person], females: Seq[Person], preference: Person => Seq[Person]): Person => Person = {
    def score(arrangement: Seq[(Person, Person)]): Int = {
      arrangement.map { case (men, female) => preference(men).indexOf(female)}.sum
    }

    val permutations: Iterator[Seq[Person]] = males.permutations
    val all: Seq[Seq[(Person, Person)]] = permutations.map(permutatedMales => permutatedMales.zip(females)).toSeq
    all.foreach(println)
    val best: Seq[(Person, Person)] = all.minBy(score)
    println(best)
    val bestFull: Seq[(Person, Person)] = best.flatMap { pair => Seq(pair,pair.swap) }
    println(bestFull)
        bestFull.toMap
//    person => best
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
  }


}
