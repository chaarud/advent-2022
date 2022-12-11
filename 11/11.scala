import scala.io.Source

@main
def main(): Unit = {
  case class Item(worryLevel: Long)
  case class Monkey(id: Int, op: Long => Long, divisibleBy: Int, ifTrue: Int, ifFalse: Int, items: List[Item], totalInspections: Long)

  def makeItems(str: String) = str.split(", ").map(_.toLong).map(Item.apply).toList

  val monkeys: Array[Monkey] = Array(
    Monkey(0, _ * 7, 19, 6, 7, makeItems("85, 77, 77"), 0),
    Monkey(1, _ * 11, 3, 3, 5, makeItems("80, 99"), 0),
    Monkey(2, _ + 8, 13, 0, 6, makeItems("74, 60, 74, 63, 86, 92, 80"), 0),
    Monkey(3, _ + 7, 7, 2, 4, makeItems("71, 58, 93, 65, 80, 68, 54, 71"), 0),
    Monkey(4, _ + 5, 5, 2, 0, makeItems("97, 56, 79, 65, 58"), 0),
    Monkey(5, _ + 4, 11, 4, 3, makeItems("77"), 0),
    Monkey(6, x => x * x, 17, 7, 1, makeItems("99, 90, 84, 50"), 0),
    Monkey(7, _ + 3, 2, 5, 1, makeItems("50, 66, 61, 92, 64, 78"), 0)
  )

  def throwItem(source: Int, target: Int, newWorryLevel: Long, monkeys: Array[Monkey]): Array[Monkey] = {
    val from = monkeys(source)
    val to = monkeys(target)
    monkeys(source) = from.copy(items = from.items.tail, totalInspections = from.totalInspections + 1)
    monkeys(target) = to.copy(items = to.items.appended(Item(newWorryLevel)))
    monkeys
  }

//  val sampleMonkeys: Array[Monkey] = Array(
//    Monkey(0, _ * 19, 23, 2, 3, makeItems("79, 98"), 0),
//    Monkey(1, _ + 6, 19, 2, 0, makeItems("54, 65, 75, 74"), 0),
//    Monkey(2, x => x*x, 13, 1, 3, makeItems("79, 60, 97"), 0),
//    Monkey(3, _ + 3, 17, 0, 1, makeItems("74"), 0)
//  )

  val totalRounds = 20
  val result: Array[Monkey] = (1 to totalRounds).foldLeft(monkeys.clone()) { (monkeys, _) =>
    (0 until 8).foldLeft(monkeys) { (monkeys, i) =>
      monkeys(i).items.foldLeft(monkeys) { (monkeys, item) =>
        val monkey = monkeys(i)
        val newWorryLevel = monkey.op(item.worryLevel) / 3
        if (newWorryLevel % monkey.divisibleBy == 0) {
          throwItem(i, monkey.ifTrue, newWorryLevel, monkeys)
        } else {
          throwItem(i, monkey.ifFalse, newWorryLevel, monkeys)
        }
      }
    }
  }
  val sorted = result.sortBy(_.totalInspections).reverse
  println(sorted.head.totalInspections * sorted.tail.head.totalInspections)

  val lcm = monkeys.map(_.divisibleBy).product
  val bigTotalRounds = 10000
  val bigResult: Array[Monkey] = (1 to bigTotalRounds).foldLeft(monkeys) { (monkeys, _) =>
    val roundResult = (0 until 8).foldLeft(monkeys) { (monkeys, i) =>
      monkeys(i).items.foldLeft(monkeys) { (monkeys, item) =>
        val monkey = monkeys(i)
        val newWorryLevel = monkey.op(item.worryLevel) % lcm
        if (newWorryLevel % monkey.divisibleBy == 0) {
          throwItem(i, monkey.ifTrue, newWorryLevel, monkeys)
        } else {
          throwItem(i, monkey.ifFalse, newWorryLevel, monkeys)
        }
      }
    }
    //if (round % 1000 == 0 || round == 1 || round == 20)
    //  println(s"Round $round: [${foo.map(_.totalInspections).mkString(", ")}]")
    roundResult
  }
  val bigSorted = bigResult.sortBy(_.totalInspections).reverse
  println(bigSorted.head.totalInspections * bigSorted.tail.head.totalInspections)
}
