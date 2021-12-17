import scala.collection.mutable
import scala.io.Source

object Day16 extends App {

  class HexToBitStream(hexSource: Source) {
    private val buffer = new mutable.Queue[Char]()
    private val hexToBits = Map(
      '0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011",
      '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
      '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011",
      'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111"
    )
    var bitsRead = 0

    def readBits(n: Int): String = {
      val res = new StringBuilder()
      while(res.length() < n) {
        if(buffer.isEmpty) {
          buffer.addAll(hexToBits(hexSource.next()))
        }
        res += buffer.dequeue()
        bitsRead += 1
      }
      res.result()
    }
  }

  def parseBinary(s: String): Long = {
    java.lang.Long.parseLong(s, 2)
  }

  trait Packet {
    val version: Int
    val subPackets: List[Packet]
    def eval(): Long
  }

  case class Literal(version: Int, value: Long) extends Packet {
    override val subPackets: List[Packet] = List.empty
    override def eval(): Long = value
  }
  case class Operator(version: Int, operatorNum: Int, subPackets: List[Packet]) extends Packet {
    override def eval(): Long = {
      val subEvals = subPackets.map(_.eval())
      operatorNum match {
        case 0 => subEvals.sum
        case 1 => subEvals.product
        case 2 => subEvals.min
        case 3 => subEvals.max
        case 5 => if(subEvals(0) > subEvals(1)) 1 else 0
        case 6 => if(subEvals(0) < subEvals(1)) 1 else 0
        case 7 => if(subEvals(0) == subEvals(1)) 1 else 0
        case _ => throw new IllegalArgumentException(s"Invalid operator number: $operatorNum")
      }
    }
  }

  val TYPE_LITERAL = 4

  val LENGTH_TYPE_N_BITS = 0
  val LENGTH_TYPE_N_PACKETS = 1

  def parsePacket(in: HexToBitStream): Packet = {
    val version = parseBinary(in.readBits(3)).toInt
    val typeId = parseBinary(in.readBits(3)).toInt
    if(typeId == TYPE_LITERAL) {
      val literal = parseLiteral(in)
      Literal(version, literal)
    } else {
      val lengthType = parseBinary(in.readBits(1)).toInt
      val subpackets = if(lengthType == LENGTH_TYPE_N_BITS) {
        val nbits = parseBinary(in.readBits(15))
        parsePacketsNBits(in, nbits)
      } else {
        assert(lengthType == LENGTH_TYPE_N_PACKETS)
        val npackets = parseBinary(in.readBits(11))
        parseNPackets(in, npackets)
      }
      Operator(version, typeId, subpackets)
    }
  }

  def parsePacketsNBits(in: HexToBitStream, nBits: Long): List[Packet] = {
    if(nBits <= 0) List.empty
    else {
      val bitsReadBefore = in.bitsRead
      val first = parsePacket(in)
      val bitsRead = in.bitsRead - bitsReadBefore
      first :: parsePacketsNBits(in, nBits - bitsRead)
    }
  }

  def parseNPackets(in: Day16.HexToBitStream, nPackets: Long): List[Packet] = {
    if(nPackets <= 0) List.empty
    else {
      parsePacket(in) :: parseNPackets(in, nPackets - 1)
    }
  }

  def parseLiteral(in: HexToBitStream): Long = {
    val literalVal = new StringBuilder()
    while(in.readBits(1) != "0") {
      literalVal.append(in.readBits(4))
    }
    literalVal.append(in.readBits(4))
    parseBinary(literalVal.result())
  }


  /*
   * Packet Header:
   * 0-3 :: Version number
   * 3-6 :: Type ID
   *
   * Type 4 -> Literal Value
   * 5-bit chunks. Last chunk starts with 0, all others start with 1.
   * right 4 bits store value.
   *
   * Other types:
   * 6-7 : Length type bit :: 0=length, 1=num packets
   */

  def sumVersions(p: Packet): Int = {
    p.version + p.subPackets.map(sumVersions).sum
  }

  def partOne(src: Source): Int = {
    val stream = new HexToBitStream(src)
    val rootPacket = parsePacket(stream)
    sumVersions(rootPacket)
  }

  def partTwo(src: Source): Long = {
    val stream = new HexToBitStream(src)
    val root = parsePacket(stream)
    root.eval()
  }


  def test(pass: => Boolean, name: String): Unit = {
    if(pass) {
      println(s"Test passed :: $name")
    } else {
      println(s"Test FAILED :: $name")
    }
  }

  // Tests
  {
    val smallHexString = "8A004A801A8002F478"
    val expectedBinary = "100010100000000001001010100000000001101010000000000000101111010001111000"
    val src = Source.fromString(smallHexString)
    val stream = new HexToBitStream(src)

    test(stream.readBits(smallHexString.length * 4) == expectedBinary, "Small hex, read all at once")
  }

  {
    val literalPacket = "D2FE28"
    val src = Source.fromString(literalPacket)
    val stream = new HexToBitStream(src)
    test(parsePacket(stream) == Literal(6, 2021), "Parse literal packet")
  }

  {
    val operatorPacket = "38006F45291200"
    val src = Source.fromString(operatorPacket)
    val stream = new HexToBitStream(src)
    val expected = Operator(1, 6, List(Literal(6, 10), Literal(2, 20)))
    test(parsePacket(stream) == expected, "Parse operator packet (nbits)")
  }

  {
    val operatorPacket = "EE00D40C823060"
    val src = Source.fromString(operatorPacket)
    val stream = new HexToBitStream(src)
    val expected = Operator(7, 3, List(Literal(2, 1), Literal(4, 2), Literal(1, 3)))
    test(parsePacket(stream) == expected, "Parse operator packet (npackets)")
  }

  // Advent of code parts
  {
    // Part One
    val src = Source.fromFile("input_files/day16.txt")
    println(s"*** Part 1 solution: ${partOne(src)}")
  }

  {
    // Part two tests
    test(partTwo(Source.fromString("C200B40A82")) == 3, "Sum")
    test(partTwo(Source.fromString("04005AC33890")) == 54, "Product")
    test(partTwo(Source.fromString("880086C3E88112")) == 7, "Min")
    test(partTwo(Source.fromString("CE00C43D881120")) == 9, "Max")
    test(partTwo(Source.fromString("D8005AC2A8F0")) == 1, "LT")
    test(partTwo(Source.fromString("F600BC2D8F")) == 0, "GT")
    test(partTwo(Source.fromString("9C005AC2F8F0")) == 0, "EQ")
    test(partTwo(Source.fromString("9C0141080250320F1802104A08")) == 1, "Sum & Product")
  }

  {
    // Part Two
    val src = Source.fromFile("input_files/day16.txt")
    println(s"*** Part 2 solution: ${partTwo(src)}")
  }

}
