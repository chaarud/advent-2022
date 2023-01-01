import scala.io.Source

@main
def main(): Unit = {
  val numbers = Util.getLines.map(_.toInt).zipWithIndex
  val N = numbers.length

  def mixNumbers(mixList: Seq[(Long, Int)], numbers: Seq[(Long, Int)]): Seq[(Long, Int)] = {
    mixList.foldLeft(numbers) { (mixedNumbers, packageToMix) =>
      val (numberToMix, _) = packageToMix
      val foundIdx = mixedNumbers.indexOf(packageToMix)
      val finalIdx = if (foundIdx + numberToMix >= 0 && foundIdx + numberToMix < N) {
        foundIdx + numberToMix
      } else if (numberToMix == 0) {
        foundIdx
      } else if (numberToMix > 0) {
        (foundIdx + numberToMix) % (N - 1)
      } else {
        ((foundIdx + numberToMix) % (N - 1)) + (N - 1)
      }
      val dropped = mixedNumbers.patch(foundIdx, Seq.empty, 1)
      val (first, last) = dropped.splitAt(finalIdx.asInstanceOf[Long].toInt)
      val newList = first ++ (packageToMix +: last)

      newList
    }
  }

  def sumAfterZero(mixedNumbers: Seq[(Long, Int)]): Long = {
    val idxOfZero = mixedNumbers.map(_._1).indexOf(0)
    mixedNumbers((idxOfZero + 1000) % N)._1 +
      mixedNumbers((idxOfZero + 2000) % N)._1 +
      mixedNumbers((idxOfZero + 3000) % N)._1
  }

  def part1(): Unit = {
    val numbersLong = numbers.map(x => (x._1.toLong, x._2))
    val mixed = mixNumbers(numbersLong, numbersLong)
    val result = sumAfterZero(mixed)
    println(s"Result: $result")
  }

  def part2(): Unit = {
    val updatedNumbers = numbers.map(_._1 * 811589153L).zipWithIndex
    val tenUpdatedNumbers = (1 to 10).foldLeft(Seq.empty[(Long, Int)])((acc, _) => acc ++ updatedNumbers)
    val mixed = mixNumbers(tenUpdatedNumbers, updatedNumbers)
    val result = sumAfterZero(mixed)
    println(s"Result: $result")
  }

  part1()
  part2()

  /**
   * A poorly performing solution, since it works by performing each movement operation one step at a time
   * (so, to mix the number -3, it would require three separate list operations). Too slow for part 2.
   *
   * @param numbers a sequence of tuples, (number to mix, some unique identifier)
   */
//  def stepByStep(numbers: Seq[(Int, Int)]): Unit = {
//    def forwardOne(packageToMix: (Int, Int), numbers: List[(Int, Int)]): List[(Int, Int)] = {
//      val foundIdx = numbers.indexOf(packageToMix)
//      val (first, last) = numbers.splitAt(foundIdx)
//      last match {
//        case thisNum :: nextNum :: rest => first ::: nextNum :: thisNum :: rest
//        case thisNum :: Nil => first.head :: thisNum :: first.tail
//        case Nil => first.head :: first.tail.last :: first.tail.init
//      }
//    }
//
//    def backwardOne(packageToMix: (Int, Int), numbers: List[(Int, Int)]): List[(Int, Int)] = {
//      val foundIdx = numbers.indexOf(packageToMix)
//      val (first, last) = numbers.splitAt(foundIdx)
//      first match {
//        case Nil => last.tail.init :+ last.head :+ last.tail.last
//        case singleElem :: Nil => last.head :: singleElem +: last.tail
//        case _ => first.init ::: last.head :: first.last +: last.tail
//      }
//    }
//
//    @scala.annotation.tailrec
//    def moveN(isForward: Boolean, steps: Int, packageToMix: (Int, Int), numbers: List[(Int, Int)]): List[(Int, Int)] = {
//      if (steps == 0) {
//        numbers
//      } else {
//        val nextNumbers = if (isForward) forwardOne(packageToMix, numbers) else backwardOne(packageToMix, numbers)
//        moveN(isForward, steps - 1, packageToMix, nextNumbers)
//      }
//    }
//
//    val mixedNumbers = numbers.foldLeft(numbers) { (mixedNumbers, packageToMix) =>
//      val (numberToMix, _) = packageToMix
//      val steps = math.abs(numberToMix)
//      numberToMix.sign match {
//        case 0 => mixedNumbers
//        case 1 => moveN(isForward = true, steps, packageToMix, mixedNumbers.toList)
//        case -1 => moveN(isForward = false, steps, packageToMix, mixedNumbers.toList)
//      }
//    }
//
//    val idxOfZero = mixedNumbers.map(_._1).indexOf(0)
//    val result =
//      mixedNumbers((idxOfZero + 1000) % N)._1 +
//        mixedNumbers((idxOfZero + 2000) % N)._1 +
//        mixedNumbers((idxOfZero + 3000) % N)._1
//    println(result)
//  }
//
//  stepByStep(numbers)

  /**
   * A solution that doesn't compile. Attempts to construct a linked list that is a ring, backed by a Map,
   * and use that for doing movement operations. Since it also requires doing an O(1) Map lookup for each
   * "hop" (ie, mixing the number -3 would require 3 operations), it won't be performant for part 2.
   */
//  def ring(): Unit = {
//    val ring = numbers.sliding(2).foldLeft(Map(numbers.last -> numbers.head)) { (linkedList, numbers) =>
//      linkedList + (numbers(0) -> numbers(1))
//    }
//
//    def printRing(N: Int, ring: Map[Int, Int]): String = {
//      @scala.annotation.tailrec
//      def foldMap(N: Int, point: Int, acc: String, ring: Map[Int, Int]): String = {
//        val successor = ring(point)
//        val newAcc = acc + ", " + successor
//        if (N == 0) acc else foldMap(N - 1, successor, newAcc, ring)
//      }
//
//      foldMap(N - 1, 0, "0", ring)
//    }
//
//    def remove(i: Int, ring: Map[Int, Int]): Map[Int, Int] = {
//      val successor = ring(i)
//      val predecessor = ring.find(_._2 == i).get._1
//      val finalRing = ring + (predecessor -> successor) - i
//      finalRing
//    }
//
//    def add(k: Int, v: Int, ring: Map[Int, Int]): Map[Int, Int] = {
//      val successor = ring(k)
//      ring + (k -> v) + (v -> successor)
//    }
//
//    @scala.annotation.tailrec
//    def step(stepForward: Boolean, stepsRemaining: Int, point: Int, ring: Map[Int, Int]): Int = {
//      if (stepForward) {
//        val successor = ring(point)
//        if (stepsRemaining == 0) point else step(stepForward, stepsRemaining - 1, successor, ring)
//      } else {
//        val predecessor = ring.find(_._2 == point).get._1
//        if (stepsRemaining == 0) point else step(stepForward, stepsRemaining - 1, predecessor, ring)
//      }
//    }
//
//    val finalRing = numbers.foldLeft(ring) { (ring, numberToMix) =>
//      println(s"Moving $numberToMix along the ring")
//      if (numberToMix != 0) {
//        val successor = ring(numberToMix)
//        val removedRing = remove(numberToMix, ring)
//        val stepForward = numberToMix > 0
//        val traversalEnd = step(stepForward, math.abs(numberToMix) - 1, successor, removedRing)
//        val added = add(traversalEnd, numberToMix, removedRing)
//        println(printRing(N, added))
//        added
//      } else {
//        ring
//      }
//    }
//  }
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}