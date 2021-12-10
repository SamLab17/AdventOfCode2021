import scala.collection.mutable
import scala.io.Source

object Day9 extends App {
  val src = Source.fromFile("input_files/day9.txt")
  val test = Source.fromString("2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n")

  val DIRECTIONS = Seq(
    Position(0, 1),
    Position(0, -1),
    Position(1, 0),
    Position(-1, 0),
  )

  println(partTwo(src))

  case class Position(row: Int, col: Int) {
    def inBoundsOf[T](x: Array[Array[T]]): Boolean = {
      row >= 0 && col >= 0 && row < x.length && col < x(row).length
    }

    def +(other: Position): Position = Position(this.row + other.row, this.col + other.col)
  }
  def getSurroundingValues(x: Array[Array[Int]], r: Int, c: Int): Seq[Int] = {
    val res = Seq.newBuilder[Int]
    if (r > 0) res += x(r-1)(c)
    if (r+1 < x.length) res += x(r+1)(c)
    if (c > 0) res += x(r)(c-1)
    if (c+1 < x(r).length) res += x(r)(c+1)
    res.result()
  }

  def getLowPoints(heightmap: Array[Array[Int]]): Seq[Position] = {
    val res = Seq.newBuilder[Position]
    for (r <- heightmap.indices) {
      for (c <- heightmap(0).indices) {
        if (heightmap(r)(c) < getSurroundingValues(heightmap, r, c).min) {
          res += Position(r, c)
        }
      }
    }
    res.result()
  }


  def partOne(src: Source): Int = {
    // Find low points
    val heightmap = src.getLines().map(_.toCharArray.map(_ - '0')).toArray
    getLowPoints(heightmap).map{case Position(r, c) => heightmap(r)(c) + 1}.sum
  }

  // DFS from starting position, bounded by 9s.
  def countBasinSize(cur: Position, heightmap: Array[Array[Int]], visited: mutable.Set[Position]): Int = {
    if((cur inBoundsOf heightmap) && !visited(cur)) {
      val curVal = heightmap(cur.row)(cur.col)
      visited += cur
      if (curVal == 9)  {
        0
      } else {
        DIRECTIONS.map(dir => {
          val newPos = cur + dir
          countBasinSize(newPos, heightmap, visited)
        }).sum + 1
      }
    } else 0
  }


  def partTwo(src: Source): Int = {
    val heightmap = src.getLines().map(_.toCharArray.map(_ - '0')).toArray
    val lowPoints = getLowPoints(heightmap)
    // Run DFS from all the low points to get their sizes
    val sizes = lowPoints.map(low => countBasinSize(low, heightmap, new mutable.HashSet[Position]()))
    sizes.sorted(Ordering[Int].reverse).take(3).product

  }

}
