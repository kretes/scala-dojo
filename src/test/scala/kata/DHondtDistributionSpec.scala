package kata

import org.specs2.mutable.Specification

class DHondtDistributionSpec extends Specification {

  "DHondt distribution" should {
    "resolve distribution equally for equal weights" in {
      val distribution = DHondtDistribution.distribute(Map("A"->10,"B"->10),2)

      distribution must beEqualTo(Map("A"->1,"B"->1))
    }

    "resolve proportional distribution" in {
      val distribution = DHondtDistribution.distribute(Map("A"->20,"B"->10),3)

      distribution must beEqualTo(Map("A"->2,"B"->1))
    }

    "resolve the most-weighted key as first" in {
      val distribution = DHondtDistribution.distribute(Map("A"->20,"B"->10),1)

      distribution must beEqualTo(Map("A"->1))
    }

    "resolve wikipedia example" in {
      val distribution = DHondtDistribution.distribute(Map("A"->720,"B"->480, "C" -> 300),8)

      distribution must beEqualTo(Map("A"->4,"B"->3,"C"->1))
    }
  }

}
