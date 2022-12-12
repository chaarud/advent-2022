import scala.io.Source
import scala.annotation.tailrec

@main
def main(): Unit = {
  val lines = Util.getLines
  val heights = "abcdefghijklmnopqrstuvwxyz"
  val numRows = lines.size
  val numCols = lines.head.length
  val emptyGrid = new Array[Array[Int]](numRows)
  val emptyRow = new Array[Int](numCols)
  var start = (-1, -1)
  var end = (-1, -1)

  val grid = lines.zipWithIndex.foldLeft(emptyGrid) { (grid, elem) =>
    val (line, rowIdx) = elem
    val rowArray = line.zipWithIndex.foldLeft(emptyRow.clone()) { (row, elem) =>
      val (char, colIdx) = elem
      val height = char match {
        case 'S' =>
          start = (rowIdx, colIdx)
          0
        case 'E' =>
          end = (rowIdx, colIdx)
          25
        case _ => heights.indexOf(char)
      }
      row.updated(colIdx, height)
    }
    grid.updated(rowIdx, rowArray)
  }

  def filterNeighbors(
    neighbors: List[(Int, Int)],
    originHeight: Int,
    queue: List[(Int, Int, Int)]
  ): List[(Int, Int)] =
    neighbors.filter { case (row, col) =>
      if (row < 0 || col < 0 || row >= numRows || col >= numCols)
        false
      else if (grid(row)(col) < originHeight - 1)
        false
      else if (queue.exists(x => x._1 == row && x._2 == col))
        false
      else
        true
    }

  def isStart: Int => Int => Boolean = row => col => row == start._1 && col == start._2
  val isElevationA: Int => Int => Boolean = row => col => grid(row)(col) == 0

  @tailrec
  def recurse(
    start: (Int, Int),
    queue: List[(Int, Int, Int)],
    baseCaseFn: Int => Int => Boolean
  ): Int = queue match {
    case (row, col, dist) :: _ if baseCaseFn(row)(col) =>
      dist
    case (row, col, dist) :: tail =>
      //println(s"traversing ($row, $col)")
      val height = grid(row)(col)
      val neighbors = List((row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1))
      val filteredNeighbors = filterNeighbors(neighbors, height, queue)
      val distancedNeighbors = filteredNeighbors.map(x => (x._1, x._2, dist + 1))
      recurse(start, tail ++ distancedNeighbors, baseCaseFn)
    case _ =>
      println("oops, ran out of queue")
      -1
  }

  val initialQueue = List((end._1, end._2, 0))

  val shortestPathFromStart = recurse(start, initialQueue, isStart)
  println(s"shortest distance from start point: $shortestPathFromStart")

  val shortestPathToA = recurse(start, initialQueue, isElevationA)
  println(s"shortest distance from end to elevation a: $shortestPathToA")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}