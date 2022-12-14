import scala.io.Source
import scala.annotation.tailrec

@main
def main(): Unit = {
  val lines = Util.getLines

  val rocks: Seq[Seq[(Int, Int)]] = lines.foldLeft(Seq.empty[Seq[(Int, Int)]]) { (rocks, line) =>
    val pointStrs = line.split(" -> ")
    val points = pointStrs.toList.map { str =>
      val pointArr = str.split(",").map(_.toInt)
      (pointArr(0), pointArr(1))
    }
    rocks :+ points
  }

  val maxCol = rocks.flatten.map(_._1).max
  val maxRow = rocks.flatten.map(_._2).max

  trait Volume
  case object Sand extends Volume
  case object Rock extends Volume
  case object Air extends Volume

  def createGrid(rocks: Seq[Seq[(Int, Int)]]): Array[Array[Volume]] = {
    // goes (column, row)
    // so it's a grid of columns, and each column is an array of Volumes
    val grid: Array[Array[Volume]] = Array.fill(maxCol * 2)(Array.fill(maxRow + 5)(Air))

    // fill the grid
    rocks.foldLeft(grid) { (grid, rock) =>
      rock.sliding(2).foldLeft(grid) { (grid, rockPointPair) =>
        val startPoint = rockPointPair.head
        val endPoint = rockPointPair.last
        val colDiff = Math.abs(startPoint._1 - endPoint._1)
        val rowDiff = Math.abs(startPoint._2 - endPoint._2)
        val colStart = Math.min(startPoint._1, endPoint._1)
        val rowStart = Math.min(startPoint._2, endPoint._2)
        val rockPoints = (0 to colDiff).flatMap { colOffset =>
          (0 to rowDiff).map { rowOffset =>
            (colStart + colOffset, rowStart + rowOffset)
          }
        }
        rockPoints.foreach { case (col, row) => grid(col)(row) = Rock }
        grid
      }
    }
  }

  def nextSandPos(sandPos: (Int, Int), grid: Array[Array[Volume]]): Option[(Int, Int)] = {
    val (col, row) = sandPos
    if (grid(col)(row + 1) == Air) {
      Some((col, row + 1))
    } else if (grid(col - 1)(row + 1) == Air) {
      Some((col - 1, row + 1))
    } else if (grid(col + 1)(row + 1) == Air) {
      Some((col + 1, row + 1))
    } else None
  }

  val sandSource = (500, 0)

  // pour the sand
  @tailrec
  def recurse(grid: Array[Array[Volume]], sandPos: (Int, Int), sandCount: Int): Int = {
    val (col, row) = sandPos
    nextSandPos(sandPos, grid) match {
      case None =>
        recurse(grid, sandSource, sandCount + 1)
      case Some((_, newRow)) if newRow > maxRow =>
        sandCount
      case Some((newCol, newRow)) =>
        grid(col)(row) = Air
        grid(newCol)(newRow) = Sand
        recurse(grid, (newCol, newRow), sandCount)
    }
  }

  def part1() = {
    val grid = createGrid(rocks)
    grid(sandSource._1)(sandSource._2) = Sand
    val answer = recurse(grid, sandSource, 0)
    println(s"answer: $answer")
  }

  @tailrec
  def recurse2(grid: Array[Array[Volume]], sandPos: (Int, Int), sandCount: Int): Int = {
    val (col, row) = sandPos
    nextSandPos(sandPos, grid) match {
      case None if sandPos == sandSource =>
        sandCount + 1
      case None =>
        recurse2(grid, sandSource, sandCount + 1)
      case Some((newCol, newRow)) =>
        grid(col)(row) = Air
        grid(newCol)(newRow) = Sand
        recurse2(grid, (newCol, newRow), sandCount)
    }
  }

  def part2() = {
    val grid = createGrid(rocks)
    grid(sandSource._1)(sandSource._2) = Sand
    val floorLevel = maxRow + 2
    for (colIdx <- grid.indices) {
      grid(colIdx)(floorLevel) = Rock
    }
    val floorAnswer = recurse2(grid, sandSource, 0)
    println(s"floor answer: $floorAnswer")
  }

  part1()
  part2()
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}