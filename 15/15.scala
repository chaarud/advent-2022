import scala.io.Source

@main
def main(): Unit = {
  val lines = Util.getLines
  val pattern = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
  val positionInfo = lines.foldLeft(List.empty[(Int, Int, Int, Int)]) { (state, line) =>
    line match {
      case pattern(sx, sy, bx, by) => (sx.toInt, sy.toInt, bx.toInt, by.toInt) :: state
      case l =>
        println(s"input not matched: $l")
        state
    }
  }
  def manhattanDistance(x1: Int, y1: Int, x2: Int, y2: Int): Int = Math.abs(x1 - x2) + Math.abs(y1 - y2)
  val posWithDistances: Seq[((Int, Int, Int, Int), Int)] = positionInfo.map(p => p -> manhattanDistance(p._1, p._2, p._3, p._4))

  def determineCoverageForRow(coverageRow: Int): Int = {
    // N = Math.abs(sy - coverageRow) - how far away our row is from a sensor
    // if it's 0 away, we have (2 * (dist - 0)) + 1
    // if it's 1 away, we have (2 * (dist - 1)) + 1
    // if it's N away, we have (2 * (dist - N)) + 1 spaces of coverage in the row
    // Important: only true as long as dist >= N
    // Return the x-intervals that are covered by each sensor: [sx - (dist - N), sx + (dist - N)]
    val xIntervals: Seq[(Int, Int)] = posWithDistances.flatMap { case ((sx, sy, _, _), sensorToBeaconDist) =>
      val rowToSensorDist = math.abs(sy - coverageRow)
      if (sensorToBeaconDist >= rowToSensorDist) {
        val offset = sensorToBeaconDist - rowToSensorDist
        Some((sx - offset, sx + offset))
      } else None
    }
    val xIntervalsMin = xIntervals.map(_._1).min
    val xIntervalsMax = xIntervals.map(_._2).max

    def intervalContainsPoint(interval: (Int, Int), point: Int): Boolean =
      point >= interval._1 && point <= interval._2

    (xIntervalsMin to xIntervalsMax).count { xCoord =>
      val coveredAlready = xIntervals.exists(interval => intervalContainsPoint(interval, xCoord))
      val knownBeaconExists = positionInfo.exists(p => p._3 == xCoord && p._4 == coverageRow)
      val result = coveredAlready && !knownBeaconExists
      result
    }
  }

  val answer = determineCoverageForRow(2000000)
  println(s"answer: $answer")

  // Key realization for part 2: If there's only 1 valid point for an unknown beacon to exist,
  // it needs to be right outside the edge of some sensor's boundary. If it were further out than that, there'd be more
  // than 1 valid point for that unknown beacon (since the point just closer to the nearest edge would also be valid).

  val sensors = positionInfo.map(p => (p._1, p._2))
  val beacons = positionInfo.map(p => (p._3, p._4))
  def isManhattanCovered(x: Int, y: Int) = posWithDistances.exists { case ((sx, sy, _, _), sensorToBeaconDist) =>
    manhattanDistance(x, y, sx, sy) <= sensorToBeaconDist
  }
  def isPointCovered(x: Int, y: Int): Boolean =
    !sensors.contains((x, y)) && !beacons.contains(x, y) && !isManhattanCovered(x, y)

  def edgeGenerator(sx: Int, sy: Int, targetDist: Int): Seq[(Int, Int)] =
    (0 to targetDist).flatMap { i =>
      val j = targetDist - i
      Set((sx + i, sy + j), (sx - i, sy + j), (sx + i, sy - j), (sx - i, sy - j))
    }

  def findTuningFrequency(minBound: Int, maxBound: Int): Long = {
    val allEdgePoints = posWithDistances.foldLeft(Seq.empty[(Int, Int)]) { (edgePoints, posWithDist) =>
      val ((sx, sy, _, _), sensorToBeaconDist) = posWithDist
      val boundedEdges = edgeGenerator(sx, sy, sensorToBeaconDist + 1)
        .filter { case (x, y) => x >= minBound && x <= maxBound && y >= minBound && y <= maxBound }
      edgePoints ++ boundedEdges
    }
    val (x, y) = allEdgePoints.filter(p => isPointCovered(p._1, p._2)).head
    (x * 4000000L) + y // Make a long to avoid overflow
  }

  val tuningFrequency = findTuningFrequency(0, 4000000)
  println(s"tuning frequency: $tuningFrequency")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}