package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

class LuncherSpec extends Specification with ThrownExpectations {

  case class Place(name:String)

  case class Preference(place:Place,value:Double)

  case class Person(name:String,preferences:Preference *)

  def luncher(person: Person*) : Place = {
    person.flatMap(_.preferences).groupBy(_.place).maxBy(_._2.map(_.value).sum)._1
  }

  "lunher" should {
    "take place with highest preference" in {

      val resultPlace: Place = luncher(Person("A",Preference(Place("ch"),0.9),Preference(Place("sa"),0.5)))

      resultPlace must beEqualTo(Place("ch"))
    }

    "take place with highest sum of preferences" in {

      val resultPlace: Place = luncher(
        Person("A",Preference(Place("ch"),0.8),Preference(Place("sa"),0.5)),
        Person("B",Preference(Place("ch"),0.1),Preference(Place("sa"),0.5))
      )

      resultPlace must beEqualTo(Place("sa"))
    }

    "take place with highest sum of preferences" in {

      val resultPlace: Place = luncher(
        Person("A",Preference(Place("ch"),0.8),Preference(Place("sa"),0.5)),
        Person("B",Preference(Place("ch"),0.1),Preference(Place("sa"),0.5))
      )

      resultPlace must beEqualTo(Place("sa"))
    }
  }


}
