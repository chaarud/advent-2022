import scala.io.Source
import scala.annotation.tailrec

@main
def main(): Unit = {
  val lines = Util.getLines
  val pairs = lines.filterNot(_.isBlank).grouped(2).map(ss => (ss.head, ss.last))

  @tailrec
  def nextElement(str: String, stack: List[Any]): List[Any] = {
    //println(s"stack for this iteration: $stack")
    if (str.isBlank) return stack
    str.head match {
      case '[' =>
        nextElement(str.tail, List.empty[Any] :: stack)
      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val number = if (str.length > 1 && str.head == '1' && str.tail.head == '0') {
          10
        } else {
          str.head.toString.toInt
        }
        val newStack = (stack.head.asInstanceOf[List[Any]] :+ number) :: stack.tail
        nextElement(str.tail, newStack)
      case ']' =>
        val newStack = if (stack.size <= 1) {
          stack.head :: Nil
        } else {
          val newHead = stack.tail.head.asInstanceOf[List[Any]] :+ stack.head
          newHead :: stack.tail.tail
        }
        nextElement(str.tail, newStack)
      case ',' =>
        nextElement(str.tail, stack)
    }
  }

  def parseList(str: String): List[Any] = {
    nextElement(str, Nil).head.asInstanceOf[List[Any]]
  }

//  println(parseList("[]"))
//  println(parseList("[[]]"))
//  println(parseList("[[3]]"))
//  println(parseList("[3,4,5,6]"))
//  println(parseList("[[3,4],[5,6]]"))
//  println(parseList("[[[2,5,[9],[],8],1,2,2,0],[],[]]"))
//  println(parseList("[[[2,5,[9],[],8],10,2,2,0],[],[]]"))

  def isOrdered(l1: List[Any], l2: List[Any], leftRemainders: List[Any], rightRemainders: List[Any]): Boolean = {
    //println(s"evaluating isOrdered: $l1 and $l2")
    (l1, l2) match {
      case (l :: Nil, r :: Nil) =>
        (l, r) match {
          case (left: Int, right: Int) =>
            if (left < right) true
            else if (right < left) false
            else isOrdered(leftRemainders, rightRemainders, Nil, Nil)
          case (left: List[_], right: Int) =>
            isOrdered(left, List(right), leftRemainders, rightRemainders)
          case (left: Int, right: List[_]) =>
            isOrdered(List(left), right, leftRemainders, rightRemainders)
          case (left: List[_], right: List[_]) =>
            isOrdered(left, right, leftRemainders, rightRemainders)
        }
      case (l :: ls, r :: rs) =>
        (l, r) match {
          case (left: Int, right: Int) =>
            if (left < right) true
            else if (right < left) false
            else isOrdered(ls, rs, leftRemainders, rightRemainders)
          case (left: List[_], right: Int) =>
            isOrdered(left, List(right), ls :: leftRemainders, rs :: rightRemainders)
          case (left: Int, right: List[_]) =>
            isOrdered(List(left), right, ls :: leftRemainders, rs :: rightRemainders)
          case (left: List[_], right: List[_]) =>
            isOrdered(left, right, ls :: leftRemainders, rs :: rightRemainders)
        }
      case (Nil, _ :: _) => true
      case (_ :: _, Nil) => false
      case (Nil, Nil) =>
        isOrdered(leftRemainders, rightRemainders, Nil, Nil)
    }
  }

  val answer = pairs.zipWithIndex.foldLeft(0) { (total, elem) =>
    val ((s1, s2), zeroBasedIdx) = elem
    val idx = zeroBasedIdx + 1
    val l1: List[Any] = parseList(s1)
    val l2: List[Any] = parseList(s2)
    val result = isOrdered(l1, l2, Nil, Nil)
    //println(s"$idx my result $result")
    if (result) total + idx else total
  }
  println(s"answer: $answer")

  // Note for part 1:
  // My solution was initially buggy, I kept getting answers in a small-ish range that were wrong. My wrong answers:
  // 5531 too low
  // 5532 wrong
  // 5610 too low
  // 5687 too high
  // 5796 wrong
  //
  // I eventually looked on reddit and compared my answers with the code provided by this commenter:
  //   https://www.reddit.com/r/adventofcode/comments/zkmyh4/2022_day_13_solutions/j044j5n/
  //   linked to this gist: https://gist.github.com/dougdonohoe/b15646fa7c801ea8f4c3a8a264dfce52
  // and discovered that my code was evaluating a single input (out of 150) incorrectly.
  // The above solution is totally different from mine, so I had to look for the bug and fix it.
  // The fix was replacing ::: with :: where you see `ls :: leftRemainders, rs :: rightRemainders` above
  //
  // I didn't need to use any help for part 2.

  val answerWithDecoders = {
    val decoder1 = "[[2]]"
    val decoder2 = "[[6]]"
    val augmentedInput = lines :+ decoder1 :+ decoder2
    val sorted = augmentedInput.filterNot(_.isBlank).sortWith { (s1, s2) =>
      val l1: List[Any] = parseList(s1)
      val l2: List[Any] = parseList(s2)
      val result = isOrdered(l1, l2, Nil, Nil)
      result
    }
    //println(sorted.mkString("\n"))
    (sorted.indexOf(decoder1) + 1) * (sorted.indexOf(decoder2) + 1)
  }
  println(s"answer with decoders: $answerWithDecoders")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}