import scala.io.Source

@main
def main(): Unit = {
  val lines = Util.getLines
  val pattern = """Valve ([A-Z]{2}) has flow rate=(\d+); tunnels lead to valves ([A-Z]{2})((, [A-Z]{2})*)""".r
  val pattern2 = """Valve ([A-Z]{2}) has flow rate=(\d+); tunnel leads to valve ([A-Z]{2})""".r

  val valves = lines.foldLeft(List.empty[(String, Int, List[String])]) { (valves, line) =>
    line match {
      case pattern(valveName, flow, firstChild, laterChildren, _) =>
        val children = firstChild :: laterChildren.split(", ").filter(!_.isBlank).toList
        valves :+ (valveName, flow.toInt, children)
      case pattern2(valveName, flow, firstChild) =>
        valves :+ (valveName, flow.toInt, List(firstChild))
      case l =>
        println(s"could not parse line $l")
        valves
    }
  }.map(v => v._1 -> (v._2, v._3)).toMap

  val timeLimit = 30
  val startingValve = "AA"

  var paths: Map[(String, String), List[String]] = Map.empty

  // FIFO
  def bfs(current: String, toVisit: List[String], path: List[String]): List[String] = {
    val (_, children) = valves(current)
    val newQueue = toVisit ++ children
    val newPath = path :+ current
    bfs(newQueue.head, newQueue.tail, newPath)
  }

  // broad algorithm:
  // - figure out the total flow for turning on any single valve
  // - is there any action that we know we'd want to take for sure?
  // - only 15 valves can be opened, max
  // - find...all paths that hit 15 valves?
  // - start at minute 30 and work backwards?
  // - transform the graph into some other representation?
  // - seems like I need to precompute what to do - there's no clear greedy way to do it step-by-step
  // - ...just brute force it


}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}