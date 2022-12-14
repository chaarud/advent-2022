import scala.io.Source
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

@main
def main(): Unit = {
  val lines = Util.getLines
  val pairs = lines.filterNot(_.isBlank).grouped(2).map(ss => (ss.head, ss.last))

//  trait Packet
//  case class IntElem(value: Int) extends Packet
//  case class ListElem(value: List[Packet]) extends Packet

  // [[4, 5], [9]]
  // Stack of either ints or stacks
  // if you see a [ then put a stack onto the stack
  // if you see an int put an int onto the stack
//  def parseList(str: String, stack: List[Any]): List[Any] = {
//    if (str.head == '[') {
////      val newStack = parseList(str.tail, List.empty[Any])
////      newStack :: stack
//      parseList(str.tail, stack)
//    } else if (str.head == ',') {
//      parseList(str.tail, stack)
//    } else {
//      val foo = str.split("]", 2)
//      val numbers = foo.head.split(",").filterNot(_.isBlank).map(_.toInt).toList
//      if (foo.last.isBlank)
//        numbers :: stack
//      else
//        parseList(foo.last, numbers :: stack)
//    }
//  }

//  @tailrec
//  def parseList(str: String, stack: List[Any]): List[Any] = {
//    if (str.isBlank) return stack
//    str.head match {
//      case ']' | ',' =>
//        parseList(str.tail, stack)
//      case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
//        val number = str.head.toString.toInt
//        val bar = stack.head.asInstanceOf[List[Any]]
//        val baz = bar :+ number
//        parseList(str.tail, baz ::: stack.tail)
//      case '[' =>
//        if (stack.isEmpty)
//          parseList(str.tail, List.empty[Any] :: stack)
//        else {
//          val newHead = List.empty[Any] :: stack.head.asInstanceOf[List[Any]]
//          parseList(str.tail, newHead :: stack.tail)
//        }
//      case _ =>
//        stack
//    }
//  }

  //  def parseList(s: String): ListElem = {
  //    if (s.head == '[') {
  //      val innerList = parseList(s.tail)
  //
  //    } else {
  //      // ...|5, 4, 3, 2|], [6, []]
  //      val foo = s.split("]", 2)
  //      val numbers = foo.head.split(",").map(_.toInt).toList
  //    }
  //  }

  @tailrec
  def nextElement(str: String, stack: List[Any]): List[Any] = {
//    println(s"stack for this iteration: $stack")
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

//  case class ReturnTrue() extends Throwable
//  case class ReturnFalse() extends Throwable


//  def isOrdered(l1: List[Any], l2: List[Any]): Boolean = {
//    println(s"evaluating isOrdered: $l1 and $l2")
//    val maxLen = Math.max(l1.length, l2.length)
//    Try {
//      (0 until maxLen).map { idx =>
//        if (idx >= l1.length) throw ReturnTrue()
//        if (idx >= l2.length) throw ReturnFalse()
//        (l1(idx), l2(idx)) match {
//          case (left: Int, right: Int) =>
//            println(s"comparing two ints: $left and $right")
//            if (left < right) throw ReturnTrue()
//            if (right < left) throw ReturnFalse()
//            -1
//          case (left: List[_], right: Int) =>
//            isOrdered(left, List(right))
//          case (left: Int, right: List[_]) =>
//            isOrdered(List(left), right)
//          case (left: List[_], right: List[_]) =>
//            isOrdered(left, right)
//        }
//      }
//    } match {
//      case Success(bs) =>
////        println("WARNING: YOU SHOULDN'T BE HERE")
////        println(bs)
//        l1.length < l2.length
//      case Failure(e) =>
//        println(s"returning $e on inputs $l1 and $l2")
//        e match {
//          case _: ReturnTrue =>
//            println("returning true!")
//            true
//          case _: ReturnFalse =>
//            println("returning false!")
//            false
//          case t =>
//            println(s"something else: $t")
//            throw t
//        }
//    }
//  }

//  println(isOrdered(List(0), List(8)))
//  println(isOrdered(List(8), List(0)))
//  println(isOrdered(List(0), List(List(8))))
//  println(isOrdered(List(8), List(List(0))))

  def isOrdered(l1: List[Any], l2: List[Any], leftRemainders: List[Any], rightRemainders: List[Any]): Boolean = {
//    println(s"evaluating isOrdered: $l1 and $l2")
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

  // Buggy solution. Validated by https://www.reddit.com/r/adventofcode/comments/zkmyh4/2022_day_13_solutions/j044j5n/
  // and https://gist.github.com/dougdonohoe/b15646fa7c801ea8f4c3a8a264dfce52
  // and discovered a single input was wrong, leading to a single bugfix (replacing ::: with :: where you see ls :: leftRemainders, rs :: rightRemainders above)

  val answer = pairs.zipWithIndex.foldLeft(0) { (total, elem) =>
    val ((s1, s2), zeroBasedIdx) = elem
    val idx = zeroBasedIdx + 1
    val l1: List[Any] = parseList(s1)
    val l2: List[Any] = parseList(s2)
    val result = isOrdered(l1, l2, Nil, Nil)
//    println(s"$idx my result $result")
    if (result) total + idx else total
  }
  // 5531 too low
  // 5532 wrong
  // 5610 too low (I think)
  // 5687 too high
  // 5796 wrong
  println(s"answer: $answer")

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
//    println(sorted.mkString("\n"))
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