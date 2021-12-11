import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.io.AnsiColor.REVERSED
import scala.io.AnsiColor.RESET


object Day11 extends App {
  val src = Source.fromFile("input_files/day11.txt")
  val smallTest = Source.fromString("11111\n19991\n19191\n19991\n11111")
  val test = Source.fromString(
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin)

  val FLASH_ENERGY = 9

//  println(partOne(src, 100))
  println(partTwo(src))

  case class Octopus(var energyLevel: Int) {
    val neighbors = new ListBuffer[Octopus]
    var flashed = false

    def addNeighbor(o: Octopus): Unit = {
      neighbors += o
    }

    def increaseEnergy(): Int = {
      if (flashed)
        0
      else {
        energyLevel += 1
        flashed = energyLevel > FLASH_ENERGY
        if (flashed)
          neighbors.filterNot(_.flashed).map(_.increaseEnergy()).sum + 1
        else
          0
      }
    }

    def finalizeStep(): Unit = {
      energyLevel %= FLASH_ENERGY + 1
      flashed = false
    }

    override def toString: String = {
      val modEnergy = energyLevel % (FLASH_ENERGY + 1)
      if (flashed) s"$REVERSED$modEnergy$RESET" else s"$modEnergy"
    }
  }

  def inBounds[T](a: Array[Array[T]], r: Int, c: Int): Boolean = {
    r >= 0 && c >= 0 && r < a.length && c < a(r).length
  }

  def assignNeighbors(oct: Array[Array[Octopus]]): Unit = {
    oct.indices.foreach(r => {
      oct(r).indices.foreach(c => {
        for (deltaR <- -1 to 1) {
          for (deltaC <- -1 to 1) {
            if ((deltaR != 0 || deltaC != 0) && inBounds(oct, r + deltaR, c + deltaC)) {
              oct(r)(c).addNeighbor(oct(r + deltaR)(c + deltaC))
            }
          }
        }
      })
    })
  }

  def printOctopuses(oct: Array[Array[Octopus]]): Unit = {
    oct.foreach(row => {
      row.foreach(o => {
        print(o)
      })
      println()
    })
    println()
  }

  // returns number of flashes
  def partOne(src: Source, nSteps: Int, printOutput: Boolean = false): Int = {
    // Build octopusses, (octopi)?
    val input = src.getLines()
      .map(line =>
        line.toCharArray.map(_ - '0').map(Octopus)
      ).toArray
    assignNeighbors(input)
    if (printOutput) {
      println("Before any steps: ")
      printOctopuses(input)
    }

    val flatInput = input.flatten

    var numFlashes = 0
    for (step <- 1 to nSteps) {
      numFlashes += flatInput.map(_.increaseEnergy()).sum
      if (printOutput) {
        println(s"After step $step:")
        printOctopuses(input)
      }
      flatInput.foreach(_.finalizeStep())
    }
    numFlashes
  }

  def partTwo(src: Source): Int = {
    val input = src.getLines()
      .map(line =>
        line.toCharArray.map(_ - '0').map(Octopus)
      ).toArray
    assignNeighbors(input)

    val flatInput = input.flatten
    val numOctopuses = flatInput.length

    var stepNum = 1
    while(true) {
      val numFlashed = flatInput.map(_.increaseEnergy()).sum
      flatInput.foreach(_.finalizeStep())
      if(numFlashed == numOctopuses)
        return stepNum
      stepNum += 1
    }
    0
  }
}
