import scala.io.Source

@main
def main() = {
  val lines = Util.getLines
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

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}