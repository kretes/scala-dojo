package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.math.Ordering.by

class JamJarsSpec extends Specification with ThrownExpectations {

  case class Jar(flavour: String, fresh: Boolean)

  case class Shelve(jars: Seq[Jar])

  case class JarBox(jars: Seq[Jar])

  def giveMeMyJars(shelves: Seq[Shelve], jarsInBox: Int): Seq[JarBox] = {
    val freshJars: Seq[Jar] =
      shelves.flatMap(_.jars)
        .filter(_.fresh)
        .sortBy(_.flavour)(Ordering[String].reverse)

    freshJars
      .sliding(size = jarsInBox, step = jarsInBox).toSeq
      .map(JarBox)
  }

  "jamjars" should {
    "return fresh jar in box" in {

      val tastyFreshJar: Jar = Jar("tasty", fresh = true)

      giveMeMyJars(Seq(Shelve(Seq(tastyFreshJar))), 1) should contain(exactly(JarBox(Seq(tastyFreshJar))))

    }

    "only return fresh jars" in {

      val tastyJar1: Jar = Jar("tasty", fresh = true)
      val staleJar: Jar = Jar("tasty", fresh = false)
      val tastyJar2: Jar = Jar("tasty", fresh = true)

      giveMeMyJars(Seq(Shelve(Seq(tastyJar1, staleJar, tastyJar2))), 3) should contain(
        exactly(JarBox(Seq(tastyJar1, tastyJar2))))

    }

    "group by specified limit" in {

      val tastyJar1: Jar = Jar("tasty", fresh = true)
      val tastyJar2: Jar = Jar("tasty", fresh = true)
      val tastyJar3: Jar = Jar("tasty", fresh = true)

      giveMeMyJars(Seq(Shelve(Seq(tastyJar1, tastyJar2, tastyJar3))), jarsInBox = 2) should contain(
        exactly(JarBox(Seq(tastyJar1, tastyJar2)), JarBox(Seq(tastyJar3))))

    }

    "sort by reversed flavour name" in {

      val tastyJar_a: Jar = Jar("a", fresh = true)
      val tastyJar_z: Jar = Jar("z", fresh = true)
      val tastyJar_k: Jar = Jar("k", fresh = true)

      giveMeMyJars(Seq(Shelve(Seq(tastyJar_a, tastyJar_z, tastyJar_k))), jarsInBox = 3) should contain(
        exactly(JarBox(Seq(tastyJar_z, tastyJar_k, tastyJar_a))))

    }

  }


}
