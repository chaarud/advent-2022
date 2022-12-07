import scala.io.Source

@main
def main() = {
  val lines = Utils.getLines("04/input")
  val pairCount = lines.count { line =>
    val pattern = """(\d+)""".r
    pattern.findAllIn(line).toSeq.map(_.toInt) match {
      case low1 :: high1 :: low2 :: high2 :: Nil =>
        if (low1 <= low2 && high1 >= high2) {
          true
        } else if (low2 <= low1 && high2 >= high1) {
          true
        } else {
          false
        }
      case otherMatch =>
        println(s"line didn't match! $otherMatch")
        false
    }
  }

  println(s"pair count: $pairCount")

  val overlapCount = lines.count { line =>
    val pattern = """(\d+)""".r
    pattern.findAllIn(line).toSeq.map(_.toInt) match {
      case low1 :: high1 :: low2 :: high2 :: Nil =>
        if (low1 <= low2 && high1 >= high2) {
          true
        } else if (low2 <= low1 && high2 >= high1) {
          true
        } else if (low1 <= low2 && high1 <= high2 && high1 >= low2) {
          true
        } else if (low2 <= low1 && high2 <= high1 && high2 >= low1) {
          true
        } else {
          false
        }
      case otherMatch =>
        println(s"line didn't match! $otherMatch")
        false
    }
  }

  println(s"overlap count: $overlapCount")
}

object Utils {
  def getLines(path: String): Seq[String] = {
    val bufferedSource = Source.fromFile(s"/Users/chaaru/Desktop/advent-2022/input/$path")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}
