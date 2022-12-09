import scala.io.Source

@main
def main = {
  def isVisible(rowIdx: Int, colIdx: Int, grid: Array[Array[Int]]): Boolean = {
    val thisHeight = grid(rowIdx)(colIdx)
    val (treesToLeft, thisPlusTreesToRight) = grid(rowIdx).splitAt(colIdx)
    val treesToRight = thisPlusTreesToRight.tail
    val treesAbove = (0 until rowIdx).map(i => grid(i)(colIdx))
    val treesBelow = (rowIdx + 1 until grid(0).length).map(i => grid(i)(colIdx))

    val visibleFromRight = treesToRight.forall(_ < thisHeight)
    val visibleFromLeft = treesToLeft.forall(_ < thisHeight)
    val visibleFromTop = treesAbove.forall(_ < thisHeight)
    val visibleFromBottom = treesBelow.forall(_ < thisHeight)

    visibleFromRight || visibleFromLeft || visibleFromTop || visibleFromBottom
  }

  val input = Util.getLines

  val grid = input.foldLeft(Array.empty[Array[Int]]) { (grid, line) =>
    grid.appended(line.map(_.toInt).toArray)
  }

  val totalVisible = grid.indices.foldLeft(0) { (count, rowIdx) =>
    grid(0).indices.foldLeft(count) { (count, colIdx) =>
      if (isVisible(rowIdx, colIdx, grid)) count + 1 else count
    }
  }

  println(s"total visible: $totalVisible")

  def getSenicScore(rowIdx: Int, colIdx: Int, grid: Array[Array[Int]]): Int = {

    def countView(myHeight: Int, heights: Seq[Int]): Int = {
      heights.foldLeft((0, false)) { (state, height) =>
        val (count, isCountStopping) = state
        if (isCountStopping) {
          state
        } else if (myHeight > height) {
          (count + 1, false)
        } else {
          (count + 1, true)
        }
      }._1
    }

    val thisHeight = grid(rowIdx)(colIdx)
    val (treesToLeft, thisPlusTreesToRight) = grid(rowIdx).splitAt(colIdx)
    val treesToRight = thisPlusTreesToRight.tail
    val treesAbove = (0 until rowIdx).map(i => grid(i)(colIdx))
    val treesBelow = (rowIdx + 1 until grid(0).length).map(i => grid(i)(colIdx))

    val scoreFromRight = countView(thisHeight, treesToRight)
    val scoreFromLeft = countView(thisHeight, treesToLeft.reverse)
    val scoreFromTop = countView(thisHeight, treesAbove.reverse)
    val scoreFromBottom = countView(thisHeight, treesBelow)

    scoreFromRight * scoreFromLeft * scoreFromTop * scoreFromBottom
  }

  val maxSenicScore = grid.indices.foldLeft(0) { (maxScore, rowIdx) =>
    grid(0).indices.foldLeft(maxScore) { (maxScore, colIdx) =>
      Math.max(getSenicScore(rowIdx, colIdx, grid), maxScore)
    }
  }

  println(s"max senic score: $maxSenicScore")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}