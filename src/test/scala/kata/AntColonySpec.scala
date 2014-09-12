package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.util.Random

class AntColonySpec extends Specification with ThrownExpectations {

  case class Cord(x: Int, y: Int) {
    def +(cord: Cord) = Cord(x + cord.x, y + cord.y)

  }

  class Food

  class Ant

  case class World(xSize: Int, ySize: Int, basePosition: Cord, ants: Seq[(Cord, Ant)]) {


    def isInWorld(cord: Cord): Boolean = (0 to xSize-1).contains(cord.x) && (0 to ySize-1).contains(cord.y)

    def randomMove(cord: Cord) = {
      val potentialMovements = Random.shuffle(Seq(Cord(1, 0), Cord(0, 1), Cord(-1, 0), Cord(0, -1)).filter(potentialMove => isInWorld(cord + potentialMove)))
      cord + potentialMovements.headOption.getOrElse(Cord(0,0))
    }

    def nextWorld = World(xSize, ySize, basePosition, ants.map { case (cord, ant) => (randomMove(cord), ant)})
  }

  object World {
    def apply(xSize: Int, ySize: Int, basePosition: Cord, antsCount: Int) =
      new World(xSize, ySize, basePosition, (0 to antsCount).map(_ => (Cord(0, 0) -> new Ant)))
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
  }


}
