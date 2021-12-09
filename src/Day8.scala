import scala.io.Source

object Day8 extends App {
  val src = Source.fromFile("input_files/day8.txt")
  val test = Source.fromString(
    """>be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      >edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      >fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      >fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      >aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      >fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      >dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      >bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      >egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      >gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
      >""".stripMargin('>'))

  val sevenSegmentToInt = Map(
    "abcefg" -> 0,
    "cf" -> 1,
    "acdeg" -> 2,
    "acdfg" -> 3,
    "bcdf" -> 4,
    "abdfg" -> 5,
    "abdefg" -> 6,
    "acf" -> 7,
    "abcdefg" -> 8,
    "abcdfg" -> 9
  )
  val intToSevenSegment = sevenSegmentToInt.map(_.swap)
  val intToNumSegments = intToSevenSegment.view.mapValues(_.length).toMap

//  println(partOne(src))
  println(partTwo(src))

  def partOne(src: Source): Int = {
    val parsed = src.getLines().map(_.split(" \\| ")).map{
      case Array(in, out) => (in.split("\\s+"), out.split("\\s+"))
      case x => throw new IllegalArgumentException(s"malformed input $x")
    }
    val uniqueLengths = Seq(2, 3, 4, 7)
    val outputs = parsed.flatMap(_._2).toList
    outputs.map(_.length).count(uniqueLengths.contains(_))
  }


  def differenceOfStrings(s1: String, s2: String): String =
    s1.filterNot(s2.contains(_))

  def intersectionOfStrings(strings: String*): String = {
    if(strings.isEmpty)
      ""
    else {
      val first = strings.head
      first.filter(c => strings.forall(_.contains(c)))
    }
  }


  def getMappings(inputs: Array[String]): Map[Char, Char] = {
    val result = Map.newBuilder[Char, Char]
    val lenToInputs = inputs.sorted.map(in => (in.length, in)).groupBy(_._1).view.mapValues(_.map(_._2))

    // Check that 1, 4, 7 are unique
    assert(lenToInputs(intToNumSegments(1)).length == 1)
    assert(lenToInputs(intToNumSegments(4)).length == 1)
    assert(lenToInputs(intToNumSegments(7)).length == 1)

    val codesCF = lenToInputs(intToNumSegments(1))(0)
    val codesBCDF = lenToInputs(intToNumSegments(4))(0)
    val codesACF = lenToInputs(intToNumSegments(7))(0)
    val codesABCDEFG = lenToInputs(intToNumSegments(8))(0)

    // Start with 1 and 7
    val codeForA = differenceOfStrings(codesACF, codesCF)
    assert(codeForA.length == 1)
    result += (codeForA(0) -> 'a')

    // The 5-length codes
    // :_* expands the Array so that it can be passed into a varargs function (gross)
    val codesADG = intersectionOfStrings(lenToInputs(5):_*) // a, d, g

    val codeForD = intersectionOfStrings(codesADG, codesBCDF)
    assert(codeForD.length == 1)
    result += (codeForD(0) -> 'd')

    val codeForG = differenceOfStrings(codesADG, codeForD + codeForA)
    assert(codeForG.length == 1)
    result += (codeForG(0) -> 'g')

    val codesBD = differenceOfStrings(codesBCDF, codesCF)
    val codeForB = differenceOfStrings(codesBD, codeForD)
    assert(codeForB.length == 1)
    result += (codeForB(0) -> 'b')

    val codesForABDG = codeForA + codeForB + codeForD + codeForG
    val lenFive = lenToInputs(5)
    val possibleCodesForFive = lenFive.filter(code => differenceOfStrings(code, codesForABDG).length == 1)
    assert(possibleCodesForFive.length == 1)
    val codeForFive = possibleCodesForFive(0)

    val codeForF = differenceOfStrings(codeForFive, codesForABDG)
    assert(codeForF.length == 1)
    result += (codeForF(0) -> 'f')

    val codeForC = differenceOfStrings(codesBCDF, codeForB + codeForD + codeForF)
    assert(codeForC.length == 1)
    result += (codeForC(0) -> 'c')

    val codeForE = differenceOfStrings(codesABCDEFG, codeForA + codeForB + codeForC + codeForD + codeForF + codeForG)
    assert(codeForE.length == 1)
    result += (codeForE(0) -> 'e')

    result.result()
  }

  def decode(outputs: Array[String], mappings: Map[Char, Char]): Array[Int] = {
    outputs.map(outputString => {
      sevenSegmentToInt(outputString.map(encodedChar => mappings(encodedChar)).sorted)
    })
  }

  def readDigits(digits: Array[Int]): Int = {
    digits.foldLeft(0)((cur, d) => cur * 10 + d)
  }

  def partTwo(src: Source): Int = {
    val parsed = src.getLines().map(_.split(" \\| ")).map{
      case Array(in, out) => (in.split("\\s+"), out.split("\\s+"))
      case x => throw new IllegalArgumentException(s"malformed input $x")
    }
    parsed.map{
      case (inputs, outputs) => readDigits(decode(outputs, getMappings(inputs)))
    }.sum
  }
}
