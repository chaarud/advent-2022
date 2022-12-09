import scala.io.Source

@main
def main() = {
  val lines = Util.getLines

  val pattern = """([URDL]) (\d+)""".r

  val headPos: (Int, Int) = (0, 0)
  val tailPos: (Int, Int) = (0, 0)
  val visited: Set[(Int, Int)] = Set(tailPos)

  def getNewPos(newHeadPos: (Int, Int), tailPos: (Int, Int)): (Int, Int) = {
    (newHeadPos._1 - tailPos._1, newHeadPos._2 - tailPos._2) match {
      // Some of these will never crop up, but just want to cover more to be safe
      case (0, 0) => tailPos
      case (1, 0) | (-1, 0) | (0, 1) | (0, -1) => tailPos
      case (1, 1) | (-1, -1) | (1, -1) | (-1, 1) => tailPos
      case (2, 0) => (tailPos._1 + 1, tailPos._2)
      case (-2, 0) => (tailPos._1 - 1, tailPos._2)
      case (0, 2) => (tailPos._1, tailPos._2 + 1)
      case (0, -2) => (tailPos._1, tailPos._2 - 1)
      case (2, 2) | (2, 1) | (1, 2) => (tailPos._1 + 1, tailPos._2 + 1)
      case (2, -2) | (2, -1) | (1, -2) => (tailPos._1 + 1, tailPos._2 - 1)
      case (-2, 2) | (-2, 1) | (-1, 2) => (tailPos._1 - 1, tailPos._2 + 1)
      case (-2, -2) | (-1, -2) | (-2, -1) => (tailPos._1 - 1, tailPos._2 - 1)
    }
  }

  val (_, _, finalVisited) = lines.foldLeft((headPos, tailPos, visited)) { (acc, line) =>
    line match {
      case pattern(direction, countS) =>
        val count = countS.toInt
        (1 to count).foldLeft(acc) { (acc, _) =>
          val (headPos, tailPos, visited) = acc
          val newHeadPos = direction match {
            case "U" => (headPos._1, headPos._2 + 1)
            case "R" => (headPos._1 + 1, headPos._2)
            case "L" => (headPos._1 - 1, headPos._2)
            case "D" => (headPos._1, headPos._2 - 1)
            case _ =>
              println(s"Unexpected direction $direction")
              headPos
          }
          val newTailPos = getNewPos(newHeadPos, tailPos)
          val newVisited = visited + newTailPos
          (newHeadPos, newTailPos, newVisited)
        }
      case l =>
        println(s"Unexpected input: $l")
        acc
    }
  }

  println(s"final visited count: ${finalVisited.size}")

  val positions = Array.fill(9)((0, 0))
  val (_, _, multiKnotFinalVisited) = lines.foldLeft((headPos, positions, visited)) { (acc, line) =>
    line match {
      case pattern(direction, countS) =>
        val count = countS.toInt
        (1 to count).foldLeft(acc) { (acc, _) =>
          val (headPos, positions, visited) = acc
          val newHeadPos = direction match {
            case "U" => (headPos._1, headPos._2 + 1)
            case "R" => (headPos._1 + 1, headPos._2)
            case "L" => (headPos._1 - 1, headPos._2)
            case "D" => (headPos._1, headPos._2 - 1)
            case _ =>
              println(s"Unexpected direction $direction")
              headPos
          }

          // TODO why doesn't this fold work?
//          val (_, finalPositions) = positions.clone().zipWithIndex.foldLeft((newHeadPos, positions)) { (acc, posWithIndex) =>
//            val (thisPos, i) = posWithIndex
//            val (newPrecursorPos, positions) = acc
//            println(s"in knot ${i+2}. Precursor pos $newPrecursorPos. This pos $thisPos.")
//            val newThisPos = getNewPos(newPrecursorPos, thisPos)
//            (newThisPos, positions.updated(i, newThisPos))
//          }

          positions.indices.foreach { i =>
            val newPrecursorPos = if (i == 0) newHeadPos else positions(i-1)
            // println(s"in knot ${i + 2}. Precursor pos $newPrecursorPos. This pos ${positions(i)}.")
            val newThisPos = getNewPos(newPrecursorPos, positions(i))
            positions(i) = newThisPos
          }

          val newVisited = visited + positions.last
          (newHeadPos, positions, newVisited)
        }
      case l =>
        println(s"Unexpected input: $l")
        acc
    }
  }

  println(s"multi-knot final visited count: ${multiKnotFinalVisited.size}")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}