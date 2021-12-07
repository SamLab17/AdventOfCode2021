import scala.collection.mutable
import scala.io.Source

object Day5 extends App {
  val src = Source.fromFile("input_files/day5.txt")

  val test = Source.fromString(
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2
      |""".stripMargin)

  // Part one
  println(getNumPoints(src, false))

  // Part two
  println(getNumPoints(src, true))


  case class Point(x: Int, y: Int)

  case class LineSegment(p1: Point, p2: Point) {
    private def getDir(x: Int, y: Int): Int = {
      if (x == y)
        0
      else if (x < y)
        1
      else
        -1
    }

    def isStraight: Boolean = {
      p1.x == p2.x || p1.y == p2.y
    }

    def onLine(p: Point) = {
      p.x >= Math.min(p1.x, p2.x) &&
        p.x <= Math.max(p1.x, p2.x) &&
        p.y >= Math.min(p1.y, p2.y) &&
        p.y <= Math.max(p1.y, p2.y)
    }

    def getPoints(): List[Point] = {
      val dirX = getDir(p1.x, p2.x)
      val dirY = getDir(p1.y, p2.y)
      var cur = p1
      val res = List.newBuilder[Point]
      while (onLine(cur)) {
        res += cur
        cur = Point(cur.x + dirX, cur.y + dirY)
      }
      res.result()
    }
  }

  def buildPoint(s: String): Point = {
    val coords = s.split(",").map(_.toInt)
    Point(coords(0), coords(1))
  }

  def buildLine(s: String): LineSegment = {
    val points = s.split(" -> ").map(buildPoint)
    LineSegment(points(0), points(1))
  }

  def getNumPoints(src: Source, considerDiagonals: Boolean): Int = {
    val covered = new mutable.HashSet[Point]
    val twoCrosses = new mutable.HashSet[Point]

    src.getLines()
      .map(buildLine)
      // considerDiagonals -> _.isStraight (implies)
      // p -> q === ~p \/ q
      .filter(!considerDiagonals || _.isStraight)
      .foreach(l => {
        l.getPoints().foreach(p => {
          if (covered.contains(p)) {
            twoCrosses += p
          } else {
            covered += p;
          }
        })
      })
    twoCrosses.size
  }

}
