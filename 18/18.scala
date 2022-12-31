import scala.io.Source

@main
def main(): Unit = {
  val lines = Util.getLines

  val droplets = lines.foldLeft(List.empty[(Int, Int, Int)]) { (droplets, line) =>
    line.split(",").toList match {
      case x :: y :: z :: Nil => (x.toInt, y.toInt, z.toInt) :: droplets
      case l =>
        println(s"could not parse line $l")
        droplets
    }
  }.toSet

  def potentialNeighbors(droplet: (Int, Int, Int)): List[(Int, Int, Int)] = {
    val (x, y, z) = droplet
    List(
      (x + 1, y, z),
      (x - 1, y, z),
      (x, y + 1, z),
      (x, y - 1, z),
      (x, y, z + 1),
      (x, y, z - 1)
    )
  }

  def countExposures(droplet: (Int, Int, Int), droplets: Set[(Int, Int, Int)]): Int =
    6 - potentialNeighbors(droplet).count(droplets.contains)

  val exposedFaces = droplets.foldLeft(0) { (acc, droplet) => acc + countExposures(droplet, droplets)}
  println(s"exposed faces: $exposedFaces")

  // start from a known exterior and build a point -> contents set. mark everything you can find via bfs/dfs as outside.
  // Iterate all points and mark everything unmarked as inside.

  val exteriorPoint = (0, 0, 0) // manually checked the input, all lava droplets are positive coordinates
  // upper bounds with a small buffer to not have to worry about off-by-ones
  val maxX = droplets.map(_._1).max + 2
  val maxY = droplets.map(_._2).max + 2
  val maxZ = droplets.map(_._3).max + 2

  sealed trait Volume
  case object Exterior extends Volume
  case object Interior extends Volume
  case object Lava extends Volume

  var mutableMap = Map.empty[(Int, Int, Int), Volume]
  mutableMap = droplets.foldLeft(mutableMap)((acc, droplet) => acc + (droplet -> Lava))

  def isOutOfBounds(droplet: (Int, Int, Int)): Boolean = {
    val (x, y, z) = droplet
    x < 0 || y < 0 || z < 0 || x > maxX || y > maxY || z > maxZ
  }

  @scala.annotation.tailrec
  def bfs(queue: List[(Int, Int, Int)]): Unit = {
    queue match {
      case droplet :: tail =>
        mutableMap = mutableMap + (droplet -> Exterior)
        val filteredNeighbors = potentialNeighbors(droplet).filter { potentialNeighbor =>
          val outOfBounds = isOutOfBounds(potentialNeighbor)
          !mutableMap.contains(potentialNeighbor) && !queue.contains(potentialNeighbor) && !outOfBounds
        }
        bfs(tail ::: filteredNeighbors)
      case _ =>
        println("done with bfs")
    }
  }

  bfs(List(exteriorPoint))

  (0 to maxX).map { x =>
    (0 to maxY).map { y =>
      (0 to maxZ).map { z =>
        mutableMap.get((x, y, z)) match {
          case None =>
            mutableMap = mutableMap + ((x, y, z) -> Interior)
          case _ =>
        }
      }
    }
  }

  def countExteriors(droplet: (Int, Int, Int), droplets: Map[(Int, Int, Int), Volume]): Int =
    6 - potentialNeighbors(droplet).count { potentialNeighbor =>
      val potentialNeighborVolume = droplets.get(potentialNeighbor)
      potentialNeighborVolume.exists(volume => volume == Interior || volume == Lava)
    }

  val exteriorFaces = droplets.foldLeft(0) { (acc, droplet) => acc + countExteriors(droplet, mutableMap) }
  println(s"exposed faces: $exteriorFaces")

}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}