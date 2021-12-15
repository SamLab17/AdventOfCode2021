import scala.collection.mutable
import scala.io.Source

object Day14 extends App {
  val src = Source.fromFile("input_files/day14.txt")
  val test = Source.fromString(
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin)

//  println(partOne(src, 10))

  println(partTwo(src, 40))

  def partOne(src: Source, nsteps: Int): Int = {
    val lines = src.getLines()
    val template = lines.next()
    lines.next() // empty line between template and rules
    val rules = lines.map(_.split(" -> ")).map {
      case Array(inputs, output) => (inputs, output(0))
    }.toMap


    var current = template
    for (_ <- 1 to nsteps) {
      val next = current.sliding(2).flatMap(pair => {
        if (rules.contains(pair))
          Seq(pair.head, rules(pair))
        else
          Seq(pair.head)
      }).mkString("")
      current = next + current.last
    }

    val frequencies = current.groupBy(identity).view.mapValues(_.size)
    val maxFreq = frequencies.values.max
    val minFreq = frequencies.values.min
    maxFreq - minFreq
  }

  def addToCount(m: mutable.HashMap[String, Long], k: String, toAdd: Long): Unit = {
    m.updateWith(k) {
      case Some(oldCount) => Some(oldCount + toAdd)
      case None => Some(toAdd)
    }
  }

  def partTwo(src: Source, nsteps: Int): Long = {
    val lines = src.getLines()
    val template = lines.next()
    lines.next() // empty line between template and rules
    val rules = lines.map(_.split(" -> ")).map {
      case Array(inputs, output) => (inputs, output(0))
    }.toMap

    val pairFrequencies = template.sliding(2)
      .toList
      .groupBy(identity)
      .view.mapValues(_.size.toLong)
      .toMap

    var curFrequencies = new mutable.HashMap[String, Long]()
    curFrequencies ++= pairFrequencies

    for (_ <- 1 to nsteps) {
      val nextFrequencies = new mutable.HashMap[String, Long]()
      curFrequencies.foreach {
        case (pair, freq) =>
          if (rules.contains(pair)) {
            val output = rules(pair).toString
            addToCount(nextFrequencies, pair(0) + output, freq)
            addToCount(nextFrequencies, output + pair(1), freq)
          } else {
            addToCount(nextFrequencies, pair, freq)
          }
      }
      curFrequencies = nextFrequencies
    }

    // Convert frequencies of pairs to frequencies of individual characters
    // Only count frequency of first character of each pair.
    // This leaves out the last character in the string. But that character
    // cannot change by applying these rules. So just add 1 to the frequency
    // of the last character in the original string.
    val frequenciesOfFirstChars = curFrequencies.toList.map {
      case (pair, freq) => (pair(0), freq)
    }
      .groupBy(_._1)
      .view.mapValues(entries => entries.map(_._2).sum)
      .toList

    val frequencies = frequenciesOfFirstChars.map{
      case (c, freq) => {
        if(c == template.last) {
          // Add one to frequency of last character since the above
          // trick of counting first of each pair never counts the last
          // character in the string.
          freq + 1
        } else {
          freq
        }
      }
    }

    frequencies.max - frequencies.min
  }
}
