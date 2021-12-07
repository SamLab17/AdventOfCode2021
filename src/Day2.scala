import scala.io.Source

object Day2 extends App {

  val src = Source.fromFile("input_files/day2.txt")
//  println(partOne(src))
  println(partTwo(src))

  def readAsTuples(src: Source): Iterator[(String, Int)] = {
    src.getLines().map(_.split(" ")).map(_.toSeq).map {
      case Seq(instr, param) => (instr, param.toInt)
    }
  }

  def partOne(src: Source): Int = {
    var depth = 0;
    var horiz = 0;
    readAsTuples(src).foreach {
      case (instr, param) =>
        instr match {
          case "forward" => horiz += param
          case "down" => depth += param
          case "up" => depth -= param
      }
    }
    depth * horiz
  }

  def partTwo(src: Source): Int = {
    var depth = 0
    var horiz = 0
    var aim = 0
    readAsTuples(src).foreach{
      case (instr, param) =>
        instr match {
          case "down" => aim += param
          case "up" => aim -= param
          case "forward" => {
            horiz += param;
            depth += param * aim;
          }
        }
    }
    horiz * depth
  }
}
