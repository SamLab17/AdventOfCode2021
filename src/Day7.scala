import scala.io.Source

object Day7 extends App {
  val src = Source.fromFile("input_files/day7.txt")

  val test = Source.fromString("16,1,2,0,4,2,7,1,2,14")
//  println(partOne(src))
  println(partTwo(src))

  def median(nums: List[Int]): Int = {
    val sorted = nums.sorted
    if(sorted.length % 2 == 0) {
      (sorted(sorted.length/2 - 1) + sorted(sorted.length / 2)) / 2
    } else {
      sorted(sorted.length / 2)
    }
  }
  def partOne(src: Source): Int = {
    val input = src.getLines().next().split(",").map(_.toInt).toList
    val med = median(input)
    input.map(pos => Math.abs(pos - med)).sum
  }

  def partTwo(src: Source): Int = {
    val input = src.getLines().next().split(",").map(_.toInt).toList
    // Needing to move N spots requires N(N+1)/2 fuel.
    var lowestCost = Int.MaxValue
    var lowestCostPos = 0
    for (i <- input.min to input.max) {
      val cost = input.map(pos => Math.abs(pos - i))
        .map(nSpots => nSpots * (nSpots + 1) / 2)
        .sum
      if(cost < lowestCost)  {
        lowestCost = cost
        lowestCostPos = i
      }
    }
    lowestCost
  }
}
