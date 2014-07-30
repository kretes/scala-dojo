package kata

object DHondtDistribution {

  def distribute(keyWeights: Map[String, Int], targetCount: Int) = {
    def resolveByDHondt(keyWeights: Map[String, VaryingWeight], countLeft: Int, result: Map[String, Int]) : Map[String, Int] = {
      if (countLeft == 0) result
      else {
        val entryWithHighestWeight: (String, VaryingWeight) = keyWeights.maxBy(entry => entry._2.weight)
        val updatedResult: Map[String, Int] = result.updated(entryWithHighestWeight._1, result(entryWithHighestWeight._1) + 1)
        resolveByDHondt(keyWeights.updated(entryWithHighestWeight._1, entryWithHighestWeight._2.dividedOnceMore),countLeft - 1, updatedResult)
      }
    }

    resolveByDHondt(keyWeights.mapValues(VaryingWeight(_,1)), targetCount, Map.empty[String,Int].withDefaultValue(0))
  }

}


case class VaryingWeight(originalWeight: Int, divisor: Int) {

  def dividedOnceMore: VaryingWeight = copy(divisor = divisor + 1)

  def weight: Double = originalWeight / divisor

}