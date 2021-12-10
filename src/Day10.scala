import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
  val test = Source.fromString(
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin)
  val src = Source.fromFile("input_files/day10.txt")

  val CLOSING = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  val POINTS = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  val PART2_POINTS = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  val PART2_POINT_FACTOR = 5

  println(partTwo(src))

  def isOpening(c: Char): Boolean = CLOSING.keySet(c)

  def isClosing(c: Char): Boolean = CLOSING.values.exists(_ == c)

  def firstIllegalChar(s: String): Option[Char] = {
    val stack = new mutable.Stack[Char]
    s.foreach(c => {
      if (isOpening(c)) {
        stack.push(c)
      } else if (isClosing(c)) {
        if (c != CLOSING(stack.pop())) {
          return Some(c)
        }
      } else {
        throw new IllegalArgumentException(s"Invalid token $c")
      }
    })
    None
  }

  def partOne(src: Source): Int = {
    src.getLines().map(line => {
      firstIllegalChar(line).map(POINTS(_)).getOrElse(0)
    }).sum
  }

  def getRemainingChars(s: String): Option[Seq[Char]] = {
    val stack = new mutable.Stack[Char]
    s.foreach(c => {
      if (isOpening(c)) {
        stack.push(c)
      } else if (isClosing(c)) {
        if (c != CLOSING(stack.pop())) {
          // Line is corrupted
          return None
        }
      } else {
        throw new IllegalArgumentException(s"Invalid token $c")
      }
    })
    if (stack.isEmpty)
      Some(Seq[Char]()) // Line is complete
    else {
      Some(stack.map(CLOSING(_)).toSeq)
    }
  }

  def calculateScore(s: Seq[Char]): Long = {
    s.foldLeft(0L)((cur, c) => cur * PART2_POINT_FACTOR + PART2_POINTS(c))
  }

  def partTwo(src: Source): Long = {
    val scores = src.getLines()
      .map(line =>
        getRemainingChars(line)
          .map(calculateScore)
      )
      .filter(_.nonEmpty)
      .map(_.get)
      .toList
      .sorted
    scores(scores.size / 2)
  }
}
