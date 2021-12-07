import scala.io.Source

object Day6 extends App {
  val src = Source.fromFile("input_files/day6.txt")
  val test = Source.fromString("3,4,3,1,2")

//  println(partOne(test, 18))
//  println(partOne(src, 80))

  // Part two simply changes the number of days.
  println(numFish(src, 256))

  def numFish(src: Source, numDays: Int): Long = {
    val MAX_TIMER = 9
    val TIMER_AFTER_REPRODUCE = 6
    var populations = Array.fill(MAX_TIMER)(0L)
    src.getLines().next().split(",").map(_.toInt).foreach(age => {
      populations(age) += 1
    })

    for(_ <- 1 to numDays) {
      val newPopulations = Array.fill(MAX_TIMER)(0L)
      for(p <- 0 until (newPopulations.length - 1)) {
        newPopulations(p) = populations(p + 1)
      }
      val reproducing = populations(0)
      newPopulations(MAX_TIMER-1) += reproducing
      newPopulations(TIMER_AFTER_REPRODUCE) += reproducing

      populations = newPopulations
    }
    populations.sum
  }
}
