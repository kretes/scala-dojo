package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.collection.generic.CanBuildFrom
import scala.util.Random
import scalaz.Cord

class AntColonySpec extends Specification with ThrownExpectations {

  sequential

  case class Cord(x: Int, y: Int) {
    def +(cord: Cord) = Cord(x + cord.x, y + cord.y)

    def strictNeighbours = Seq(this + Cord(1, 0), this + Cord(-1, 0), this + Cord(0, 1), this + Cord(0, -1))
  }

  class Food

  class Ant {
    def withFood: AntWithFood = AntWithFood(1)
  }

  case class AntWithFood(turns: Int) {

    def pheromoneLevel: Double = 1.0 / Math.pow(2.0, turns * 2)

    def nextTurn: AntWithFood = AntWithFood(turns + 1)

    def leaveFood = new Ant
  }

  var shuffleFunction: Seq[Cord] => Seq[Cord] = Random.shuffle _

  case class World(xSize: Int, ySize: Int, basePosition: Cord, ants: Seq[(Cord, Ant)], antsWithFood: Seq[(Cord, AntWithFood)], foods: Set[Cord], pheromones: Map[Cord, Double]) {

    def pheromoneStrengthAt(cord: Cord) = pheromones.getOrElse(cord, 0.0)

    def isInWorld(cord: Cord): Boolean = (0 to xSize - 1).contains(cord.x) && (0 to ySize - 1).contains(cord.y)

    def isFoodAt(cord: Cord): Boolean = foods.contains(cord)

    def randomMove(cord: Cord) = {
      val potentialMovement: Option[Cord] = shuffleFunction(Seq(Cord(1, 0), Cord(0, 1), Cord(-1, 0), Cord(0, -1))).find(potentialMove => isInWorld(cord + potentialMove))
      cord + potentialMovement.getOrElse(Cord(0, 0))
    }

    def addValues(map1: Map[Cord, Double], map2: Map[Cord, Double]): Map[Cord, Double] = {
      map2 ++ map1.map { case (cord, value) => (cord, value + map2.getOrElse(cord, 0.0))}
    }

    def neighbourWithHighestPheromoneOrFood(cord: Cord): Cord = {
      cord.strictNeighbours.find(isFoodAt).getOrElse(
        if (cord.strictNeighbours.map(pheromoneStrengthAt).sum > 0.0) cord.strictNeighbours.maxBy(pheromoneStrengthAt) else randomMove(cord)
      )
    }

    def next = {
      def antTakingFood[T]: ((Cord, T)) => Boolean = {
        case (cord, ant) => foods.contains(cord)
      }
      def antPheromoneMove[T]: ((Cord, T)) => (Cord, T) = {
        case (cord, ant) => (neighbourWithHighestPheromoneOrFood(cord), ant)
      }
      def antRandomMove[T]: ((Cord, T)) => (Cord, T) = {
        case (cord, ant) => (randomMove(cord), ant)
      }
      def toAntWithFood: ((Cord, Ant)) => (Cord, AntWithFood) = {
        case (cord, ant) => (cord, ant.withFood)
      }
      def nextTurn: ((Cord, AntWithFood)) => (Cord, AntWithFood) = {
        case (cord, antWithFood) => (cord, antWithFood.nextTurn)
      }
      def toCurrentPheromoneLevel: ((Cord, AntWithFood)) => (Cord, Double) = {
        case (cord, antWithFood) => (cord, antWithFood.pheromoneLevel)
      }
      def antLeaveFood: ((Cord, AntWithFood)) => (Cord, Ant) = {
        case (cord, antWithFood) => (cord, antWithFood.leaveFood)
      }
      val antInBase: ((Cord, Any)) => Boolean = {
        case (cord, ant) => cord == basePosition
      }
      val movedAnts: Seq[(Cord, Ant)] = (ants.filterNot(antTakingFood) ++ antsWithFood.filter(antInBase).map(antLeaveFood)) map antPheromoneMove
      val newAntsWithFood: Seq[(Cord, AntWithFood)] = (antsWithFood.filterNot(antInBase).map(nextTurn) ++ ants.filter(antTakingFood).map(toAntWithFood)) map antRandomMove
      val foodEaten: Set[Cord] = foods.filterNot(ants.filter(antTakingFood).toMap.keySet.contains)
      val newPheromones: Map[Cord, Double] = addValues(antsWithFood.map(toCurrentPheromoneLevel).toMap, ants.filter(antTakingFood).map(_._1 -> 1.0).toMap) filterNot antInBase
      val pheromonesInAir: Map[Cord, Double] = addValues(newPheromones, pheromones.mapValues(_ / 2.0))
      World(xSize, ySize, basePosition, movedAnts, newAntsWithFood, foodEaten, pheromonesInAir)
    }
  }

  object World {
    def apply(xSize: Int, ySize: Int, basePosition: Cord, antsCount: Int, foodPositions: Set[Cord] = Set.empty) = {
      new World(xSize, ySize, basePosition, (0 to antsCount - 1).map(_ => basePosition -> new Ant), Seq.empty, foodPositions, Map.empty)
    }
  }

  "ants" should {
    "be in base at first" in {
      val world = World(xSize = 1, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.ants.toMap.keys must contain(Cord(0, 0))
    }

    "not move outside of world" in {
      val world = World(xSize = 1, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.next.ants.toMap.keys must contain(Cord(0, 0))
    }

    "move to only allowed position" in {
      val world = World(xSize = 2, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.next.ants.toMap.keys must contain(Cord(1, 0))
    }

    "be able to sniff food" in {
      val world = World(xSize = 3, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(2, 0)))

      world.isFoodAt(Cord(0, 0)) must beFalse
      world.isFoodAt(Cord(1, 0)) must beFalse
      world.isFoodAt(Cord(2, 0)) must beTrue
    }

    "take food away" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 2, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(1, 0)))

      val nextWorld: World = world.next

      shuffleFunction = { _ => Seq(Cord(-1, 0))}

      val finalWorld: World = nextWorld.next

      finalWorld.isFoodAt(Cord(0, 0)) must beFalse
      finalWorld.isFoodAt(Cord(1, 0)) must beFalse
    }

    "take food and go back" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 3, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(2, 0)))

      val nextWorld: World = world.next.next

      shuffleFunction = { _ => Seq(Cord(-1, 0))}

      val finalWorld: World = nextWorld.next

      finalWorld.antsWithFood.toMap.keys must contain(Cord(1, 0))
      finalWorld.pheromoneStrengthAt(Cord(2, 0)) must beEqualTo(1.0)
    }

    "take food and return to base" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 3, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(2, 0)))

      val nextWorld: World = world.next.next

      shuffleFunction = { _ => Seq(Cord(-1, 0))}

      val finalWorld: World = nextWorld.next.next

      finalWorld.antsWithFood.toMap.keys must contain(Cord(0, 0))
      finalWorld.pheromoneStrengthAt(Cord(1, 0)) must beEqualTo(0.25)

    }

    "pheromone should evaporate" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 3, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(2, 0)))

      val nextWorld: World = world.next.next
      shuffleFunction = { _ => Seq(Cord(-1, 0))}
      val finalWorld: World = nextWorld.next.next
      finalWorld.pheromoneStrengthAt(Cord(1, 0)) must beEqualTo(1.0 / 4)
      finalWorld.next.pheromoneStrengthAt(Cord(1, 0)) must beEqualTo(1.0 / 8)
      finalWorld.next.next.pheromoneStrengthAt(Cord(1, 0)) must beEqualTo(1.0 / 16)
    }

    "pheromone should be additive" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 4, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(2, 0), Cord(3, 0)))

      val food1Taken: World = world.next.next
      shuffleFunction = { _ => Seq(Cord(-1, 0))}
      val food1Returned: World = food1Taken.next.next
      shuffleFunction = { seq => seq}
      val food2Taken: World = food1Returned.next.next.next
      shuffleFunction = { _ => Seq(Cord(-1, 0))}
      val food2Returned: World = food2Taken.next.next.next
      food2Returned.pheromoneStrengthAt(Cord(1, 0)) must beEqualTo(1.0 / 256 + 1.0 / 16)
    }

    "leave food in base" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 2, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(1, 0)))

      val nextWorld: World = world.next.next.next

      nextWorld.antsWithFood must beEmpty
      nextWorld.ants.toMap.keys must contain(Cord(1, 0))
    }

    "follow pheromones" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 3, ySize = 3, basePosition = Cord(1, 1), antsCount = 1, foodPositions = Set(Cord(2, 2)))

      val nextWorld: World = world.next.next.next
      shuffleFunction = { _ => Seq(Cord(0, -1))}
      val finalWorld: World = nextWorld.next.next

      finalWorld.ants.toMap.keys must contain(Cord(1, 2))
    }

    "two ants go with food over the same place - pheromones should be added" in pending
    "two ants are on a position with food - only one should take food" in pending

  }


}
