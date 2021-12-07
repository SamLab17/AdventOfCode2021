import scala.io.Source
import scala.io.AnsiColor.REVERSED
import scala.io.AnsiColor.RESET

object Day4 extends App {
  val src = Source.fromFile("input_files/day4.txt")
  val test = Source.fromFile("test_files/day4.txt")

  val BOARD_SIZE = 5

  //  println(partOne(src))
  println(partTwo(src))

  case class Board(rawBoard: Seq[String]) {

    case class BoardCell(value: Int, var called: Boolean)

    private val board: Seq[Seq[BoardCell]] = rawBoard.map(s => {
      s.split("\\s+")
        .filter(_.nonEmpty)
        .map(digits => BoardCell(digits.toInt, called = false))
        .toSeq
    })

    def findNumber(n: Int): Option[(Int, Int)] = {
      board.indices.foreach(row => {
        board(row).indices.foreach(col => {
          if (board(row)(col).value == n) {
            return Some((row, col))
          }
        })
      })
      None
    }

    def checkForWin(row: Int, col: Int): Boolean = {
      // Horizontal || Vertical
      board(row).forall(_.called) || board.indices.forall(i => board(i)(col).called)
    }

    def getScore(n: Int): Int = {
      board.map(b =>
        b.filter(!_.called)
          .map(_.value)
          .sum
      )
        .sum * n
    }

    def callNumber(n: Int): Option[Int] = {
      findNumber(n) match {
        case Some((row, col)) =>
          board(row)(col).called = true
          if (checkForWin(row, col)) Some(getScore(n)) else None

        case None => None
      }
    }

    override def toString: String = {
      val sb = new StringBuilder()
      board.foreach(row => {
        row.foreach(cell => {
          val cellVal = String.format("%3d", cell.value)
          if (cell.called) {
            sb.append(s"$REVERSED$cellVal$RESET")
          } else {
            sb.append(cellVal)
          }
          sb.append(" ")
        })
        sb.append("\n")
      })
      sb.toString
    }
  }

  def buildBoards(rawBoards: Iterator[String]): Seq[Board] =
    rawBoards.grouped(BOARD_SIZE).toList.map(b => Board(b))


  def partOne(src: Source): Int = {
    val lines = src.getLines().filter(_.nonEmpty)
    val callouts = lines.next().split(",").map(_.toInt)
    val boards = buildBoards(lines)

    callouts.foreach(called => {
      boards.foreach(b => {
        b.callNumber(called) match {
          case Some(score) => return score;
          case _ =>
        }
      })
    })
    -1
  }

  def partTwo(src: Source): Int = {
    val lines = src.getLines().filter(_.nonEmpty)
    var callouts = lines.next().split(",").map(_.toInt)
    var boards = buildBoards(lines)

    // Eliminate all but one board
    while (boards.size > 1) {
      val call = callouts.head
      boards = boards.filter(b => b.callNumber(call).isEmpty)
      callouts = callouts.tail
    }
    // Get score of last board
    val last = boards.head
    while (callouts.nonEmpty) {
      last.callNumber(callouts.head) match {
        case Some(score) => return score;
        case _ => callouts = callouts.tail
      }
    }
    -1
  }

}
