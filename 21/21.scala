import scala.io.Source

@main
def main(): Unit = {
  val lines = Util.getLines

  val numberPattern = """([a-z]{4}): (\d+)""".r
  val opPattern = """([a-z]{4}): ([a-z]{4}) ([*\-+/]) ([a-z]{4})""".r

  sealed trait MonkeyAction
  case class MonkeyNum(n: Long) extends MonkeyAction
  case class MonkeyOp(left: String, right: String, op: String) extends MonkeyAction

  val monkeys: Map[String, MonkeyAction] = lines.foldLeft(Map.empty[String, MonkeyAction]) { (acc, line) =>
    line match {
      case numberPattern(id, number) => acc + (id -> MonkeyNum(number.toLong))
      case opPattern(id, left, op, right) => acc + (id -> MonkeyOp(left, right, op))
      case l =>
        println(s"couldn't extract pattern from line! $l")
        acc
    }
  }

  val rootId = "root"
  val humanId = "humn"

  def part1(): Long = {
    def doOp(op: MonkeyOp, completedMonkeys: Map[String, Long]): Option[Long] = {
      val leftResolution = completedMonkeys.get(op.left)
      val rightResolution = completedMonkeys.get(op.right)
      leftResolution.flatMap { l =>
        rightResolution.map { r =>
          op.op match {
            case "+" => l + r
            case "-" => l - r
            case "*" => l * r
            case "/" => l / r
          }
        }
      }
    }

    def loopThroughMonkeys(completedMonkeys: Map[String, Long]): Map[String, Long] = {
      monkeys.foldLeft(completedMonkeys) { (completedMonkeys, monkey) =>
        val (id, action) = monkey
        action match {
          case MonkeyNum(n) => completedMonkeys + (id -> n)
          case op: MonkeyOp =>
            val resolution = doOp(op, completedMonkeys)
            resolution.map(n => completedMonkeys + (id -> n)).getOrElse(completedMonkeys)
        }
      }
    }

    @scala.annotation.tailrec
    def recurse(completedMonkeys: Map[String, Long]): Long = {
      val newCompleted = loopThroughMonkeys(completedMonkeys)
      newCompleted.get(rootId) match {
        case Some(n) => n
        case _ => recurse(newCompleted)
      }
    }

    recurse(Map.empty)
  }

  def part2(): Long = {
    sealed trait WrappedResult
    case class WrappedVal(n: Long) extends WrappedResult
    case class WrappedPath(trail: List[(String, MonkeyOp, Option[(String, Long)])]) extends WrappedResult

    def doOp(id: String, op: MonkeyOp, completedMonkeys: Map[String, WrappedResult]): Option[WrappedResult] = {
      val leftResolution = completedMonkeys.get(op.left)
      val rightResolution = completedMonkeys.get(op.right)
      leftResolution.flatMap { lWrapped =>
        rightResolution.map { rWrapped =>
          (lWrapped, rWrapped) match {
            case (WrappedVal(l), WrappedVal(r)) =>
              val result = op.op match {
                case "+" => l + r
                case "-" => l - r
                case "*" => l * r
                case "/" => l / r
              }
              WrappedVal(result)
            case (WrappedPath(path), WrappedVal(r)) =>
              WrappedPath((id, op, Some(op.right, r)) :: path)
            case (WrappedVal(l), WrappedPath(path)) =>
              WrappedPath((id, op, Some(op.left, l)) :: path)
            case (WrappedPath(lPath), WrappedPath(rPath)) =>
              println("this should never happen!")
              WrappedPath(lPath ::: rPath)
          }
        }
      }
    }

    def loopThroughMonkeys(acc: Map[String, WrappedResult]): Map[String, WrappedResult] = {
      monkeys.foldLeft(acc) { (acc, monkey) =>
        val (id, action) = monkey
        action match {
          case MonkeyNum(n) if id == humanId =>
            val initialPath = List((humanId, MonkeyOp("", "", ""), Some(humanId, n)))
            acc + (id -> WrappedPath(initialPath))
          case MonkeyNum(n) => acc + (id -> WrappedVal(n))
          case op: MonkeyOp =>
            val resolution = doOp(id, op, acc)
            val newCompletedMonkeys = resolution.map(n => acc + (id -> n)).getOrElse(acc)
            newCompletedMonkeys
        }
      }
    }

    @scala.annotation.tailrec
    def recurse(completedMonkeys: Map[String, WrappedResult]): WrappedResult = {
      val newCompleted = loopThroughMonkeys(completedMonkeys)
      newCompleted.get(rootId) match {
        case Some(n) => n
        case _ => recurse(newCompleted)
      }
    }

    val pathWithRoot = recurse(Map.empty).asInstanceOf[WrappedPath].trail
    val rootNumber = pathWithRoot.head._3.get._2
    pathWithRoot.tail.foldLeft(rootNumber) { (N, pathElement) =>
      val (_, op, valOpt) = pathElement
      val (knownId, knownN) = valOpt.get
      op.op match {
        case "*" => N / knownN
        case "+" => N - knownN
        case "-" => if (op.right == knownId) N + knownN else -N + knownN
        case "/" => if (op.right == knownId) N * knownN else knownN / N
        case _ => N
      }
    }
  }

  println(part1())
  println(part2())
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}