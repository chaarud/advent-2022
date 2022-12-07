import scala.io.Source

@main
def main() = {
  val bufferedSource = Source.fromFile("/Users/chaaru/Desktop/advent-2022/input/01/input")
  val lines = bufferedSource.getLines().iterator.to(Seq)
  bufferedSource.close

  println(s"input length: ${lines.length}")

  val (_, summed) = lines.foldLeft((0, List.empty[Int])) { case ((currTotal, totals), line) =>
    if (!line.isBlank) {
      (line.toInt + currTotal, totals)
    } else {
      (0, currTotal :: totals)
    }
  }

  println(s"summed length: ${summed.length}")
  println(s"max: ${summed.max}")

  val sorted = summed.sortWith(_ > _)
  println(s"top 3 sum: ${sorted(0) + sorted(1) + sorted(2)}")
}
