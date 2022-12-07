import scala.io.Source

@main
def main() = {
  val lines = Utils.getLines("03/input")
  val total = lines.foldLeft(0) { (total, line) =>
    val items = line.toCharArray
    val (firstHalf, secondHalf) = items.splitAt(items.size / 2)
    val sharedItem: Char = firstHalf.find(secondHalf.contains).get
    val priority = if (sharedItem.isUpper) { sharedItem.toInt - 38 } else { sharedItem.toInt - 96 }
    total + priority
  }

  println(s"total: $total")

  val badgeTotal = lines.grouped(3).foldLeft(0) { (total, lines) =>
    val badgeType = lines.head.find(c => lines.forall(line => line.contains(c))).get
    val badgePriority = if (badgeType.isUpper) { badgeType.toInt - 38 } else { badgeType.toInt - 96 }
    total + badgePriority
  }

  println(s"badge total: $badgeTotal")

}

object Utils {
  def getLines(path: String): Seq[String] = {
    val bufferedSource = Source.fromFile(s"/Users/chaaru/Desktop/advent-2022/input/$path")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}
