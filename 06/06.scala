@main
def main = {
  val input = Util.getLines.head
  def findUniqueRuns(n: Int): Int = {
    val (first, last) = input.splitAt(n-1)
    last.zipWithIndex.foldLeft(first.toCharArray.toSeq.reverse) { (charSeq, acc) =>
      val (char, position) = acc
      if ((charSeq.toSet + char).size == n) {
        return position + n
      } else {
        char +: charSeq.dropRight(1)
      }
    }
    -1
  }

  println(s"Part 1: ${findUniqueRuns(4)}")
  println(s"Part 2: ${findUniqueRuns(14)}")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}