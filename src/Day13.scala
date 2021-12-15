import scala.io.Source

object Day13 extends App {
  val src = Source.fromFile("input_files/day13.txt")
  val test = Source.fromString(
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin)
//  println(partOne(src))

  partTwo(src)

  case class Point(x: Int, y: Int)

  def applyFold(points: Set[Point], fold: (Char, Int)): Set[Point] = {
    val axis = fold._1
    val foldLocation = fold._2
    if (axis == 'x') {
      points.map(p => {
        if (p.x > foldLocation) {
          Point(foldLocation - (p.x - foldLocation), p.y)
        } else {
          p
        }
      }).toSet
    } else if (axis == 'y') {
      points.map(p => {
        if (p.y > foldLocation) {
          Point(p.x, foldLocation - (p.y - foldLocation))
        } else {
          p
        }
      })
    } else
      throw new IllegalArgumentException(s"Invalid fold: $fold")
  }

  def parseInput(src: Source): (Set[Point], List[(Char, Int)]) = {
    val lines = src.getLines().toList
    val splitIndex = lines.indexWhere(_.isEmpty)
    val points = lines.take(splitIndex).map(_.split(",").map(_.toInt)).map {
      case Array(x, y) => Point(x, y)
    }.toSet
    val folds = lines.takeRight(lines.size - splitIndex - 1).map(s => {
      val equalIndex = s.indexOf("=")
      val axis = s(equalIndex - 1)
      val value = s.substring(equalIndex + 1).toInt
      (axis, value)
    })
    (points, folds)
  }

  def partOne(src: Source): Int = {

    val (points, folds) = parseInput(src)

    // Part One
    applyFold(points, folds.head).size
    // Part Two
    //    folds.foreach(f => points = applyFold(points, f))
    points.size
  }

  def partTwo(src: Source): Unit = {
    var (points, folds) = parseInput(src)

    folds.foreach(f => points = applyFold(points, f))
    // Print Points
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        if (points.contains(Point(x, y)))
          print("#")
        else
          print(" ")
      }
      println()
    }
  }
}
