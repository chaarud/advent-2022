import scala.io.Source

@main
def main() = {
  val lines = Utils.getLines("03/input")
}

object Utils {
  def getLines(path: String): Seq[String] = {
    val bufferedSource = Source.fromFile(s"/Users/chaaru/Desktop/advent-2022/input/$path")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}
