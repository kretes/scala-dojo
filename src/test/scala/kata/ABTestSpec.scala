package kata

import org.specs2.mutable.Specification

class ABTestSpec extends Specification {

  case class Variant(name: String, ratio: Int)

  case class ABTest(variants: Seq[Variant]) {
    def variantForId(id: Int) = {
      val ratios: Seq[Int] = variants.map(variant => variant.ratio)
      val summedRatios: Seq[Int] = ratios.scan(0)((elementA,elementB) => elementA + elementB).tail
      val a = id % ratios.sum
      variants.zip(summedRatios).find(variantWithSummedRatio => variantWithSummedRatio._2 > a).get._1
    }
  }

  "AB test" should {
    "pick the right scenario" in {
      val abTest = ABTest(Seq(Variant("A", 10), Variant("B", 20)))

      abTest.variantForId(123).name must beEqualTo("A")
      abTest.variantForId(133).name must beEqualTo("B")
    }
  }

}
