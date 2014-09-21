package kata

object DHondtDistribution {

  def distribute[A](keyWeights: Map[A, Int], targetCount: Int): Map[A, Int] = {
    def resolveByDHondt(keyWeights: Map[A, VaryingWeight], countLeft: Int, result: Map[A, Int]) : Map[A, Int] = {
      if (countLeft == 0) result
      else {
        val entryWithHighestWeight: (A, VaryingWeight) = keyWeights.maxBy(entry => entry._2.weight)
        val updatedResult: Map[A, Int] = result.updated(entryWithHighestWeight._1, result(entryWithHighestWeight._1) + 1)
        resolveByDHondt(keyWeights.updated(entryWithHighestWeight._1, entryWithHighestWeight._2.dividedOnceMore),countLeft - 1, updatedResult)
      }
    }

    resolveByDHondt(keyWeights.mapValues(VaryingWeight(_,1)), targetCount, Map.empty[A,Int].withDefaultValue(0))
  }

}


case class VaryingWeight(originalWeight: Int, divisor: Int) {

  def dividedOnceMore: VaryingWeight = copy(divisor = divisor + 1)

  def weight: Double = originalWeight / divisor

}