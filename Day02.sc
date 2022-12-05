import scala.collection.mutable
import scala.io.Source

val filename = "/Users/hendra.saputra/dev/bolo/aoc2022/Day02.txt"
val source = Source.fromFile(filename)

val RockValue: String = "A"
val PaperValue: String = "B"
val ScissorValue: String = "C"

trait ShapeType extends Comparable[ShapeType] {
  val value: String
  val lost: String
  val draw: String
  val win: String

  def score: Int = {
    value match {
      case RockValue => 1
      case PaperValue => 2
      case ScissorValue => 3
      case "X" => 1
      case "Y" => 2
      case "Z" => 3
    }
  }

  def scoreAgainst(other: ShapeType): Int = {
    if (this.compareTo(other) > 0) score + 6
    else if (this.compareTo(other) == 0) score + 3
    else score
  }

  def describeScore(other: ShapeType): String = {
    if (this.compareTo(other) > 0) s"W ($score + 6)"
    else if (this.compareTo(other) == 0) s"D ($score + 3)"
    else s"L ($score + 0)"
  }
}

trait RockTrait extends ShapeType {
  override val draw = RockValue
  override val lost = PaperValue
  override val win = ScissorValue
}
trait PaperTrait extends ShapeType{
  override val draw = PaperValue
  override val lost = ScissorValue
  override val win = RockValue
}
trait ScissorTrait extends ShapeType{
  override val draw = ScissorValue
  override val lost = RockValue
  override val win = PaperValue
}

case class Rock(override val value: String) extends RockTrait {
  override def compareTo(o: ShapeType): Int = {
    o match {
      case _: RockTrait => 0
      case _: PaperTrait => -1
      case _: ScissorTrait => 1
    }
  }
}

case class Paper(override val value: String) extends PaperTrait {
  override def compareTo(o: ShapeType): Int = {
    o match {
      case _: RockTrait => 1
      case _: PaperTrait => 0
      case _: ScissorTrait => -1
    }
  }
}

case class Scissor(override val value: String) extends ScissorTrait {
  override def compareTo(o: ShapeType): Int = {
    o match {
      case _: RockTrait => -1
      case _: PaperTrait => 1
      case _: ScissorTrait => 0
    }
  }
}

object ShapeType {
  def apply(value: String): ShapeType = value.toUpperCase().trim match {
    case RockValue => Rock(value)
    case PaperValue => Paper(value)
    case ScissorValue => Scissor(value)
    case "X" => Rock(value)
    case "Y" => Paper(value)
    case "Z" => Scissor(value)
    case _ => throw new IllegalArgumentException(s"Unknown value $value")
  }

  def pickShape(them: ShapeType, outcome: String): ShapeType = outcome.toUpperCase().trim match {
    case "X" => ShapeType(them.win)   // Mine lost
    case "Y" => ShapeType(them.draw)  // draw
    case "Z" => ShapeType(them.lost)  // Mine win
  }
}

val scores = mutable.ListBuffer.empty[Int]
for (line <- source.getLines) {
  line match {
    case s"$left $outcome" =>
      val them = ShapeType(left)
      val mine = ShapeType.pickShape(them, outcome)
      val score = mine.scoreAgainst(them)
      scores.addOne(score)

      println(s"$outcome ${mine.describeScore(them)}: $them vs $mine")
    case _ =>
      println(s"Unable to parse $line")
  }
}
source.close()

println(s"Total scores: ${scores.sum}")
