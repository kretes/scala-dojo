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

  }

  class Food

  class Ant

  //idea
  trait CountingSet[A] {
    def put(a:A) : CountingSet[A]
    def count(a:A) : Int
    def remove(a:A) : CountingSet[A]
  }

  var shuffleFunction: Seq[Cord] => Seq[Cord] = Random.shuffle _

  case class World(xSize: Int, ySize: Int, basePosition: Cord, ants: Seq[(Cord,Ant)], antsWithFood: Seq[(Cord, Ant)], foods: Set[Cord], pheromones: Map[Cord, Double]) {

    def pheromoneStrengthAt(cord: Cord) = pheromones.getOrElse(cord,0)

    def isInWorld(cord: Cord): Boolean = (0 to xSize - 1).contains(cord.x) && (0 to ySize - 1).contains(cord.y)

    def isFoodAt(cord: Cord): Boolean = foods.contains(cord)

    def randomMove(cord: Cord) = {
      val potentialMovements = shuffleFunction(Seq(Cord(1, 0), Cord(0, 1), Cord(-1, 0), Cord(0, -1)).filter(potentialMove => isInWorld(cord + potentialMove)))
      cord + potentialMovements.headOption.getOrElse(Cord(0, 0))
    }

    def nextWorld = {
      val antEatingFood: ((Cord, Ant)) => Boolean = {
        case (cord, ant) => foods.contains(cord)
      }
      val antRandomMove: ((Cord, Ant)) => (Cord, Ant) = {
        case (cord, ant) => (randomMove(cord), ant)
      }
      val movedAnts: Seq[(Cord, Ant)] = ants.filterNot(antEatingFood) map antRandomMove
      val newAntsWithFood: Seq[(Cord, Ant)] = (antsWithFood ++ ants.filter(antEatingFood)) map antRandomMove
      val foodEaten: Set[Cord] = foods.filterNot(ants.filter(antEatingFood).toMap.keySet.contains)
      val pheromonesInAir: Map[Cord, Double] = antsWithFood.map(_._1 -> 1.0).toMap
      World(xSize, ySize, basePosition, movedAnts, newAntsWithFood, foodEaten, pheromonesInAir)
    }
  }

  object World {
    def apply(xSize: Int, ySize: Int, basePosition: Cord, antsCount: Int, foodPositions: Set[Cord] = Set.empty) = {
      new World(xSize, ySize, basePosition, (0 to antsCount-1).map(_ => (Cord(0, 0) -> new Ant)), Seq.empty, foodPositions, Map.empty)
    }
  }

  "ants" should {
    "be in base at first" in {
      val world = World(xSize = 1, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.ants.toMap.keys must contain(Cord(0, 0))
    }

    "not move outside of world" in {
      val world = World(xSize = 1, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.nextWorld.ants.toMap.keys must contain(Cord(0, 0))
    }

    "move to only allowed position" in {
      val world = World(xSize = 2, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.nextWorld.ants.toMap.keys must contain(Cord(1, 0))
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

      val nextWorld: World = world.nextWorld

      shuffleFunction = { _ => Seq(Cord(-1, 0))}

      val finalWorld: World = nextWorld.nextWorld

      finalWorld.isFoodAt(Cord(0, 0)) must beFalse
      finalWorld.isFoodAt(Cord(1, 0)) must beFalse
    }

    "take food and return to base" in {
      shuffleFunction = { seq => seq}
      val world = World(xSize = 3, ySize = 1, basePosition = Cord(0, 0), antsCount = 1, foodPositions = Set(Cord(2, 0)))

      val nextWorld: World = world.nextWorld.nextWorld

      shuffleFunction = { _ => Seq(Cord(-1, 0))}

      val finalWorld: World = nextWorld.nextWorld.nextWorld

      finalWorld.antsWithFood.toMap.keys must contain(Cord(0, 0))
      finalWorld.pheromoneStrengthAt(Cord(1,0)) must beEqualTo(1.0)

    }
  }


}
