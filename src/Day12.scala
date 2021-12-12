import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day12 extends App {
  val src = Source.fromFile("input_files/day12.txt")
  val smallTest = Source.fromString("start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
  val largeTest = Source.fromString("fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")

//  println(partOne(src))

  println(partTwo(src))

  case class Cave(name: String) {
    val edges = new ListBuffer[Cave]()
    val isSmall: Boolean = name.toLowerCase == name
    val isEnd: Boolean = name == "end"
    var visitedCount: Int = 0
  }

  def countPaths(cur: Cave): Int = {
    if(cur.isEnd)
      1
    else {
      cur.edges
        .filter(dest => !(dest.isSmall && dest.visitedCount > 0))
        .map(dest => {
          cur.visitedCount += 1
          val ret = countPaths(dest)
          cur.visitedCount -= 1
          ret
        }).sum
    }
  }

  // Builds an undirected graph from the input.
  // Stores graph in a map, although if graph is fully connected,
  // a single reference to the "start" Cave should be sufficient.
  def buildGraph(src: Source): mutable.HashMap[String, Cave] = {
    val caves = new mutable.HashMap[String, Cave]
    src.getLines().map(_.split("-")).foreach{
      case Array(source, dest) => {
        val srcCave = caves.getOrElseUpdate(source, Cave(source))
        val destCave = caves.getOrElseUpdate(dest, Cave(dest))
        srcCave.edges += destCave
        destCave.edges += srcCave
      }
      case _ => throw new IllegalArgumentException("Invalid line encountered.")
    }
    caves
  }

  def partOne(src: Source): Int = {
    countPaths(buildGraph(src)("start"))
  }

  def canGoToCave(dest: Cave, doubleVisited: Boolean): Boolean = {
    if(dest.name == "start") false
    else if(dest.isSmall) {
      if(doubleVisited) dest.visitedCount == 0 else dest.visitedCount <= 1
    } else {
      true
    }
  }

  // "doubleVisited" - whether or not we've used up our double visit for a small cave.
  def countPathsPartTwo(cave: Day12.Cave, doubleVisited: Boolean): Int = {
    if(cave.isEnd) {
      1
    } else {
      cave.edges
        .filter(canGoToCave(_, doubleVisited))
        .map(dest => {
          dest.visitedCount += 1
          val doubleVisitedNow = doubleVisited || (dest.isSmall && dest.visitedCount == 2)
          val count = countPathsPartTwo(dest, doubleVisitedNow)
          dest.visitedCount -= 1
          count
      }).sum
    }
  }

  def partTwo(src: Source): Int = {
    countPathsPartTwo(buildGraph(src)("start"), doubleVisited = false)
  }
}
