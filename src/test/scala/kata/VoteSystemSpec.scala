package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

class VoteSystemSpec extends Specification with ThrownExpectations {

  case class Voivoidship(name: String)

  case class Candidate(name: String)

  case class Party(name: String)

  def calculatePlaces(voivoidships: Seq[Voivoidship], parties: Seq[Party],
                      vote: (Voivoidship) => (Candidate) => Int,
                      limit: (Voivoidship) => Int,
                      candidatesInParty: (Party) => Seq[Candidate]): (Party) => Seq[Candidate] = {
    def calculatePlaces(voivoidship: Voivoidship) : (Party) => Seq[Candidate] = {
      val votesInVoivoidship: (Candidate) => Int = vote(voivoidship)
      val votesPerParty: Map[Party, Int] = parties.map(party => (party, candidatesInParty(party).map(votesInVoivoidship).sum)).toMap
      val placesPerParty: Map[Party, Int] = DHondtDistribution.distribute(votesPerParty,limit(voivoidship))
      placesPerParty.map {case (party,places) => (party,candidatesInParty(party).sortBy(-votesInVoivoidship(_)).take(places))}.toMap.withDefaultValue(Seq.empty)
    }
    val placesInAllVoivoidships: Seq[(Party) => Seq[Candidate]] = voivoidships.map(voivoidship => calculatePlaces(voivoidship))
    aParty => placesInAllVoivoidships.map(_(aParty)).flatten
  }

  "voter" should {
    "calculate for one voivoidship one party" in {
      val maz: Voivoidship = Voivoidship("maz")
      val pp: Party = Party("PP")
      calculatePlaces(
        Seq(maz),
        Seq(pp),
        Map(maz -> Map(Candidate("1") -> 20, Candidate("2") -> 40)),
        Map(maz -> 1),
        Map(pp -> Seq(Candidate("1"), Candidate("2")))
      )(pp) must beEqualTo(Seq(Candidate("2")))
    }

    "calculate for one voivoidship two parties" in {
      val maz: Voivoidship = Voivoidship("maz")
      val p1: Party = Party("P1")
      val p2: Party = Party("P2")
      val results: (Party) => Seq[Candidate] = calculatePlaces(
        Seq(maz),
        Seq(p1, p2),
        Map(maz -> Map(Candidate("1") -> 20, Candidate("2") -> 40)),
        Map(maz -> 1),
        Map(p1 -> Seq(Candidate("1")), p2 -> Seq(Candidate("2")))
      )
      results(p2) must beEqualTo(Seq(Candidate("2")))
      results(p1) must beEmpty
    }

    "calculate for one voivoidship two parties when total votes for party wins" in {
      val maz: Voivoidship = Voivoidship("maz")
      val p1: Party = Party("P1")
      val p2: Party = Party("P2")
      val results: (Party) => Seq[Candidate] = calculatePlaces(
        Seq(maz),
        Seq(p1, p2),
        Map(maz -> Map(Candidate("1") -> 30, Candidate("11") -> 25, Candidate("2") -> 40)),
        Map(maz -> 1),
        Map(p1 -> Seq(Candidate("1"), Candidate("11")), p2 -> Seq(Candidate("2")))
      )
      results(p1) must beEqualTo(Seq(Candidate("1")))
      results(p2) must beEmpty
    }

    "calculate multiple places with DHondt method" in {
      val maz: Voivoidship = Voivoidship("maz")
      val p1: Party = Party("P1")
      val p2: Party = Party("P2")
      val results: (Party) => Seq[Candidate] = calculatePlaces(
        Seq(maz),
        Seq(p1, p2),
        Map(maz -> Map(Candidate("1") -> 31, Candidate("11") -> 29, Candidate("2") -> 40)),
        Map(maz -> 2),
        Map(p1 -> Seq(Candidate("1"), Candidate("11")), p2 -> Seq(Candidate("2")))
      )
      results(p1) must beEqualTo(Seq(Candidate("1")))
      results(p2) must beEqualTo(Seq(Candidate("2")))
    }
  }


}
