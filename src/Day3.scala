import scala.io.Source

object Day3 extends App{

  val src = Source.fromFile("input_files/day3.txt")
  val test = Source.fromString(
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010
      |""".stripMargin)

//  println(partOne(test))
//  println(partOne(src))
//  println(partTwo(test))
  println(partTwo(src))

  def modeBit(bits: List[Int]): Int = {
    val len = bits.length * 1.0
    val sum = bits.sum
    if (sum / len >= 0.5) 1 else 0
  }

  def fromBinary(bits: List[(Int, Int)]): Int = {
    bits.sortBy(_._1).map(_._2).foldLeft(0)((b1, b2) => (b1 * 2) + b2)
  }

  def inverse(bits: List[(Int, Int)]): List[(Int, Int)] = {
    bits.map{
      case (index, bit) => (index, 1-bit)
    }
  }



  def partOne(src: Source): Int = {
    val modes = src.getLines()
      .flatMap(_.view.zipWithIndex)
      .toList
      .groupBy(_._2)
      .view.mapValues(_.map(_._1 - '0'))
      .mapValues(modeBit)

    val gamma = fromBinary(modes.toList)
    val epsilon = fromBinary(inverse(modes.toList))
    gamma * epsilon
  }

  def modeBitOfColumn(lines: List[String], col: Int): Int = {
    modeBit(lines.map(_.charAt(col)).map(_ - '0'))
  }

  def oxygenRating(lines: List[String]): Int = {
    var curCol = 0
    var linesLeft = lines
    while(linesLeft.size > 1) {
      val mode = modeBitOfColumn(linesLeft, curCol)
      linesLeft = linesLeft.filter(_.charAt(curCol) - '0' == mode)
      curCol += 1
    }
    Integer.parseInt(linesLeft.head, 2)
  }

  def co2Rating(lines: List[String]): Int = {
    var curCol = 0
    var linesLeft = lines
    while(linesLeft.size > 1) {
      val leastOccurring = 1 - modeBitOfColumn(linesLeft, curCol)
      linesLeft = linesLeft.filter(_.charAt(curCol) - '0' == leastOccurring)
      curCol += 1
    }
    Integer.parseInt(linesLeft.head, 2)
  }

  def partTwo(src: Source): Int = {
    val lines = src.getLines().toList
    oxygenRating(lines) * co2Rating(lines)
  }
}
