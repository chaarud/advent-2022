import scala.io.Source

@main
def main = {
  val input = Util.getLines

  /*
  [N]     [Q]         [N]
  [R]     [F] [Q]     [G] [M]
  [J]     [Z] [T]     [R] [H] [J]
  [T] [H] [G] [R]     [B] [N] [T]
  [Z] [J] [J] [G] [F] [Z] [S] [M]
  [B] [N] [N] [N] [Q] [W] [L] [Q] [S]
  [D] [S] [R] [V] [T] [C] [C] [N] [G]
  [F] [R] [C] [F] [L] [Q] [F] [D] [P]
   1   2   3   4   5   6   7   8   9
   */
  val stacks: Array[List[String]] = Array.tabulate(9) {
    case 0 => List("n", "r", "j", "t", "z", "b", "d", "f")
    case 1 => List("h", "j", "n", "s", "r")
    case 2 => List("q", "f", "z", "g", "j", "n", "r", "c")
    case 3 => List("q", "t", "r", "g", "n", "v", "f")
    case 4 => List("f", "q", "t", "l")
    case 5 => List("n", "g", "r", "b", "z", "w", "c", "q")
    case 6 => List("m", "h", "n", "s", "l", "c", "f")
    case 7 => List("j", "t", "m", "q", "n", "d")
    case 8 => List("s", "g", "p")
  }

  val pattern = """move (\d+) from (\d+) to (\d+)""".r

  val finalStacks = input.foldLeft(stacks.clone()) { (stacks, line) =>
    line match {
      case pattern(moveS, fromS, toS) =>
        val move = moveS.toInt
        val from = fromS.toInt - 1
        val to = toS.toInt - 1
        val fromStack = stacks(from)
        val toStack = stacks(to)
        val (moving, newFrom) = fromStack.splitAt(move)
        val newTo = toStack.reverse_:::(moving)
        stacks(from) = newFrom
        stacks(to) = newTo
        stacks
      case l =>
        println(s"couldn't extract pattern from line! $l")
        stacks
    }
  }

  println(s"Final output: ${finalStacks.map(_.head).toSeq.mkString("").toUpperCase}")

  val multiMoveStacks = input.foldLeft(stacks.clone()) { (stacks, line) =>
    line match {
      case pattern(moveS, fromS, toS) =>
        val move = moveS.toInt
        val from = fromS.toInt - 1
        val to = toS.toInt - 1
        val fromStack = stacks(from)
        val toStack = stacks(to)
        val (moving, newFrom) = fromStack.splitAt(move)
        val newTo = toStack.:::(moving)
        stacks(from) = newFrom
        stacks(to) = newTo
        stacks
      case l =>
        println(s"couldn't extract pattern from line! $l")
        stacks
    }
  }

  println(s"Multi-move output: ${multiMoveStacks.map(_.head).toSeq.mkString("").toUpperCase}")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}