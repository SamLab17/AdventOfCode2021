import scala.collection.mutable
import scala.io.Source

object Day15 extends App {
  val src = Source.fromFile("input_files/day15.txt")
  val test = Source.fromString("""1163751742
                                 |1381373672
                                 |2136511328
                                 |3694931569
                                 |7463417111
                                 |1319128137
                                 |1359912421
                                 |3125421639
                                 |1293138521
                                 |2311944581""".stripMargin)
  println(partTwo(src))

  def inBounds[T](a: Array[Array[T]])(p: Pos): Boolean =
    p.row >= 0 && p.col >= 0 && p.row < a.length && p.col < a(p.row).length

  case class Pos(row: Int, col: Int) {
    def in[T](a: Array[Array[T]]): T =
      a(row)(col)
    def neighbors: Seq[Pos] =
      Seq(Pos(row-1, col), Pos(row+1, col), Pos(row, col-1), Pos(row, col+1))
  }

  case class Path(dest: Pos, cost: Int) extends Ordered[Path] {
    override def compare(that: Path): Int = that.cost compare cost
  }

  def dijkstra(a: Array[Array[Int]], end: Pos): Int = {
    val visited = new mutable.HashSet[Pos]()
    val costTo = new mutable.HashMap[Pos, Int]()
    val pq = new mutable.PriorityQueue[Path]()
    pq += Path(Pos(0, 0), 0)
    while(pq.nonEmpty) {
      val cur = pq.dequeue()
      if (!visited(cur.dest)) {
        visited += cur.dest
        costTo.put(cur.dest, cur.cost)

        if (cur.dest equals end) {
          return cur.cost
        }

        cur.dest.neighbors.filter(inBounds(a)).foreach(n => {
          val newCostToN = cur.cost + n.in(a)
          val oldCostToN = costTo.get(n)
          if (oldCostToN.isEmpty || oldCostToN.get > newCostToN) {
            costTo.put(n, newCostToN)
            pq += Path(n, newCostToN)
          }
        })
      }
    }
    throw new IllegalStateException(s"No path found to $end")
  }

  def partOne(src: Source): Int = {
    val risks = src.getLines().map(_.toCharArray.map(_ - '0')).toArray
    dijkstra(risks, Pos(risks.indices.last, risks(0).indices.last))
//    minRisk(risks)
  }

  def partTwo(src: Source): Int = {
    val input = src.getLines().map(_.toCharArray.map(_ - '0')).toArray
    val numRows = input.length
    val numCols = input(0).length
    val SCALE = 5
    val scaledInput = Array.fill(numRows * SCALE, numCols * SCALE)(0)
    // Build scaled map
    for(rRepeat <- 0 until SCALE) {
      for(cRepeat <- 0 until SCALE) {
        for(r <- input.indices) {
          for(c <- input.indices) {
            var curVal = input(r)(c) + rRepeat + cRepeat
            while(curVal > 9) {
              curVal -= 9
            }
            scaledInput(r + numRows * rRepeat)(c + numCols * cRepeat) = curVal
          }
        }
      }
    }
    dijkstra(scaledInput, Pos(scaledInput.indices.last, scaledInput(0).indices.last))
  }
}
