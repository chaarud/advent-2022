import scala.io.Source

@main
def main() = {
  val input = Util.getLines

  val lsPattern = """\$ ls""".r
  val cdRootPattern = """\$ cd /""".r
  val cdBackPattern = """\$ cd ..""".r
  val cdForwardPattern = """\$ cd ([a-zA-Z]+)""".r
  val dirPattern = """dir ([a-zA-Z]+)""".r
  val filePattern = """(\d+) ([a-zA-Z.]+)""".r

  val (files, _) = input.foldLeft((List.empty[(String, Int, List[String])], List.empty[String])) { (state, line) =>
    val (files, currentPath) = state
    //  println(s"current line: [$line]. current files tracked: ${files.size}. current path: [${currentPath.mkString(",")}]")
    line match {
      case lsPattern() => state
      case cdRootPattern() => (files, List("/"))
      case cdBackPattern() => (files, currentPath.tail)
      case cdForwardPattern(dirName) => (files, (currentPath.head + dirName) :: currentPath)
      case dirPattern(_) => state
      case filePattern(size, fileName) => ((fileName, size.toInt, currentPath) :: files, currentPath)
      case l =>
        println(s"unknown input line: $l")
        state
    }
  }

  //println(s"sample of files: ${files.take(10).mkString("\n")}")

  val dirSizes = files.foldLeft(Map.empty[String, Int]) { (dirs, file) =>
    val (_, fileSize, containingDirs) = file
    containingDirs.foldLeft(dirs) { (dirs, containingDir) =>
      val containingDirSize: Int = dirs.getOrElse(containingDir, 0) + fileSize
      dirs + (containingDir -> containingDirSize)
    }
  }

  //println(s"sample of dirs: ${dirSizes.toSeq.sortBy(_._2).take(30).mkString("\n")}")

  val sum = dirSizes.toSeq.map(_._2).filter(_ <= 100000).sum

  println(s"sum: $sum")

  val spaceTotal = 70000000
  val spaceNeeded = 30000000
  val spaceUsed = dirSizes("/")
  val spaceFree = spaceTotal - spaceUsed
  val spaceToClear = spaceNeeded - spaceFree

  val dirToDelete = dirSizes.toSeq.sortBy(_._2).find(_._2 >= spaceToClear)

  //println(s"${dirSizes.toSeq.sortBy(_._2).slice(0, 10).mkString("\n")}")

  println(s"size of directory to delete: ${dirToDelete.get._2}")
}

object Util {
  def getLines: Seq[String] = {
    val bufferedSource = Source.fromFile(s"input")
    val lines = bufferedSource.getLines().iterator.to(Seq)
    bufferedSource.close
    lines
  }
}