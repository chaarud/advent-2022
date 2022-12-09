import scala.io.Source

@main
def main() = {
  val lines = Util.getLines

  def outcomeScore(theirs: Char, mine: Char): Int = {
    (theirs, mine) match {
      case ('A', 'X') | ('B', 'Y') | ('C', 'Z') => 3
      case ('A', 'Y') | ('B', 'Z') | ('C', 'X') => 6
      case ('A', 'Z') | ('B', 'X') | ('C', 'Y') => 0
    }
  }

  def moveScore(move: Char): Int = move match {
    case 'X' => 1
    case 'Y' => 2
    case 'Z' => 3
  }

  val totalScore = lines.foldLeft(0) { (score: Int, line: String) =>
    val tokens = line.split(" ")
    val theirMove = tokens(0).head
    val myMove = tokens(1).head
    score + outcomeScore(theirMove, myMove) + moveScore(myMove)
  }

  println(s"total score: ${totalScore}")

  def getMyMove(theirMove: Char, outcome: Char): Char = {
    (theirMove, outcome) match {
      case ('A', 'Y') | ('B', 'X') | ('C', 'Z') => 'X'
      case ('B', 'Y') | ('A', 'Z') | ('C', 'X') => 'Y'
      case ('C', 'Y') | ('A', 'X') | ('B', 'Z') => 'Z'
    }
  }

  val newScore = lines.foldLeft(0) { (score: Int, line: String) =>
    val tokens = line.split(" ")
    val theirMove = tokens(0).head
    val outcome = tokens(1).head
    val myMove = getMyMove(theirMove, outcome)
    score + outcomeScore(theirMove, myMove) + moveScore(myMove)
  }

  println(s"new score: ${newScore}")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}