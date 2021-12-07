import scala.io.Source;

object Day1 extends App {
  val fileName = "input_files/day1.txt"
  val file = Source.fromFile(fileName)

//  println(partOne(file))
  println(partTwo(file))

  def partOne(src: Source): Integer = {
    src.getLines()
      .map(_.toInt)
      .sliding(2)
      .count {
        case Seq(a, b) => a < b
      }
  }

  def partTwo(src: Source): Integer = {
    src.getLines()
      .map(_.toInt)
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count {
        case Seq(a, b) => a < b
      }
  }
}
