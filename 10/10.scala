import scala.io.Source

@main
def main(): Unit = {
  val lines = Util.getLines
  val pattern = """addx (-?\d+)""".r
  val samplePoints = Set(20, 60, 100, 140, 180, 220)

  def additionalStrength(acc: (Int, Int, Int)): Int = {
    val (tick, register, _) = acc
    if (samplePoints.contains(tick)) {
      tick * register
    } else 0
  }

  val signalStrength = lines.foldLeft((1, 1, 0)) { (acc, line) =>
    val (tick, register, signalStrength) = acc
    line match {
      case pattern(n) =>
        val s1 = additionalStrength(acc)
        val afterOneTick = (tick + 1, register, signalStrength + s1)
        val s2 = additionalStrength(afterOneTick)
        (tick + 2, register + n.toInt, signalStrength + s1 + s2)
      case "noop" =>
        (tick + 1, register, signalStrength + additionalStrength(acc))
      case l =>
        println(s"could not parse line $l")
        acc
    }
  }

  println(s"Signal strength: ${signalStrength._3}")

  def updateScreen(tick: Int, register: Int, screen: Array[Array[String]]): Array[Array[String]] = {
    val row = (tick - 1) / 40
    val col = (tick - 1) % 40
    if (col == register || col - 1 == register || col + 1 == register) {
      screen(row)(col) = "██"
    } else {
      screen(row)(col) = "  "
    }
    screen
  }

  val screen = Array.fill(6)(Array.fill(40)(""))
  val finalScreen = lines.foldLeft((1, 1, screen)) { (acc, line) =>
    val (tick, register, screen) = acc
    line match {
      case pattern(n) =>
        val (nextTick, nextRegister, nextScreen) = (tick + 1, register, updateScreen(tick, register, screen))
        (nextTick + 1, nextRegister + n.toInt, updateScreen(nextTick, nextRegister, nextScreen))
      case "noop" =>
        (tick + 1, register, updateScreen(tick, register, screen))
      case l =>
        println(s"could not parse line $l")
        acc
    }
  }

  println("final screen:")
  println(finalScreen._3.map(_.mkString("")).mkString("\n"))
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}