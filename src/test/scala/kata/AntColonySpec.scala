package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

import scala.util.Random

class AntColonySpec extends Specification with ThrownExpectations {

  case class Cord(x: Int, y: Int)

  class Food

  class Ant

  case class World(xSize: Int, ySize: Int, basePosition: Cord, ants: Seq[(Cord, Ant)]) {

    def randomMove(cord: Cord) = {
      val (xChange,yChange) = Random.nextInt(4) match {
        case 0 => (1,0)
        case 1 => (0,1)
        case 2 => (-1,0)
        case 3 => (0,-1)
      }
      cord.copy(x=Math.max(0,Math.min(cord.x+xChange,xSize-1)), y=Math.max(0,Math.min(cord.y+yChange,ySize-1)))
    }

    def nextWorld = World(xSize,ySize,basePosition,ants.map {case (cord,ant) => (randomMove(cord),ant)})
  }

  object World {
    def apply(xSize: Int, ySize: Int, basePosition: Cord, antsCount: Int) =
      new World(xSize, ySize, basePosition, (0 to antsCount).map(_ => (Cord(0, 0) -> new Ant)))
  }

  "ants" should {
    "be in base at first" in {
      val world = World(xSize = 1, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.ants.toMap.keys must contain(Cord(0,0))
    }

    "not move outside of world" in {
      val world = World(xSize = 1, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.nextWorld.ants.toMap.keys must contain(Cord(0,0))
    }

    "move to only allowed position" in {
      val world = World(xSize = 2, ySize = 1, basePosition = Cord(0, 0), antsCount = 1)

      world.nextWorld.ants.toMap.keys must contain(Cord(1,0))
    }
  }


}
