package kata

import org.specs2.mutable.Specification

import scala.annotation.tailrec

/**
 * https://www.hackerrank.com/challenges/missing-numbers-fp
 */
class MissingNumbersSpec extends Specification {

  @tailrec
  private def findMissingRec(a: Seq[Int], b: Seq[Int], result: Seq[Int]):Seq[Int] = b match {
    case head +: tail => if (b.headOption.contains(head)) findMissingRec(a.tail,tail, result) else findMissingRec(a,tail, result :+ head)
    case _ => result
  }

  def findMissing(a: Seq[Int], b: Seq[Int]) =
    findMissingRec(a,b,Seq.empty)

  "missing numbers" should {
    "be found" in {
      findMissing("203 204 205 206 207 208 203 204 205 206".split(" ").toSeq.map(_.toInt),
      "203 204 204 205 206 207 205 208 203 206 205 206 204".split(" ").toSeq.map(_.toInt)) must beEqualTo(Seq(204,205,206))
    }
  }

}
