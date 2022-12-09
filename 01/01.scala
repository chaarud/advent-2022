import scala.io.Source

@main
def main() = {
  val lines = Util.getLines

  val (_, summed) = lines.foldLeft((0, List.empty[Int])) { case ((currTotal, totals), line) =>
    if (!line.isBlank) {
      (line.toInt + currTotal, totals)
    } else {
      (0, currTotal :: totals)
    }
  }

  println(s"max: ${summed.max}")

  val sorted = summed.sortWith(_ > _)
  println(s"top 3 sum: ${sorted(0) + sorted(1) + sorted(2)}")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}