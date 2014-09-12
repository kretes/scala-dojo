package kata

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification

class GameOfLifeSpec extends Specification with ThrownExpectations {

  case class Cord(x: Int, y: Int) {
    def +(cord: Cord) = Cord(x + cord.x, y + cord.y)
  }

  case class Rectangle(minCord: Cord, maxCord: Cord) {
    def extend(size: Int) = Rectangle(minCord + Cord(-size, -size), maxCord + Cord(size, size))

    def cords = for {x <- minCord.x to maxCord.x
                     y <- minCord.y to maxCord.y} yield Cord(x, y)

    def include(cord: Cord): Rectangle = {
      Rectangle(Cord(Math.min(minCord.x, cord.x), Math.min(minCord.y, cord.y)), Cord(Math.max(maxCord.x, cord.x), Math.max(maxCord.y, cord.y)))
    }
  }

  case class World(alive: Set[Cord]) {

    def neighbours(cord: Cord) = Seq(cord + Cord(1, 0), cord + Cord(-1, 0), cord + Cord(0, 1), cord + Cord(0, -1), cord + Cord(1, 1), cord + Cord(1, -1), cord + Cord(-1, 1), cord + Cord(-1, -1))

    def willBeAlive(cord: Cord): Boolean = (2 to 3).contains(aliveNeighboursCount(cord))

    def aliveNeighboursCount(cord: Cord): Int = neighbours(cord).count(alive.contains)

    def surroundings = alive.foldLeft(Rectangle(Cord(0, 0), Cord(0, 0))) { (rectangle, cord) => rectangle.include(cord)}.extend(1).cords

    def next = World(alive.filter(willBeAlive) ++ surroundings.filter(aliveNeighboursCount(_) == 3))

    def cellAlive(cord: Cord) = alive.contains(cord)
  }

  object World {

    def apply(aliveCellCords: Cord*) = new World(aliveCellCords.toSet)
  }

  "game" should {
    "say wether cell is alive" in {
      val world = World()

      world.cellAlive(Cord(0, 0)) must beFalse
    }

    "keep alive cell" in {
      val world = World(Cord(0, 0))

      world.cellAlive(Cord(0, 0)) must beTrue
    }

    "kill alive cell if no neighbours" in {
      val world = World(Cord(0, 0))

      world.next.cellAlive(Cord(0, 0)) must beFalse
    }

    "keep cell alive when two neighbours" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0))

      world.next.cellAlive(Cord(0, 0)) must beTrue
    }

    "keep cell alive when three neighbours" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0), Cord(1, 1))

      world.next.cellAlive(Cord(0, 0)) must beTrue
    }

    "die when four neighbours" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0), Cord(0, 1), Cord(0, -1))

      world.next.cellAlive(Cord(0, 0)) must beFalse
    }

    "three alive cells reproduce new cell" in {
      val world = World(Cord(-1, 0), Cord(1, 0), Cord(0, 1))

      world.next.cellAlive(Cord(0, 0)) must beTrue
    }

    "two alive cells will not reproduce new cell" in {
      val world = World(Cord(-1, 0), Cord(0, 1))

      world.next.cellAlive(Cord(0, 0)) must beFalse
    }

    "three alive cells ina row reproduce new cells" in {
      val world = World(Cord(-1, 0), Cord(0, 0), Cord(1, 0))

      world.next.cellAlive(Cord(0, 1)) must beTrue
      world.next.cellAlive(Cord(0, -1)) must beTrue
    }
  }


}
