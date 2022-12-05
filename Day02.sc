import scala.collection.mutable
import scala.io.Source

val filename = "/Users/hendra.saputra/dev/bolo/aoc2022/Day02.txt"
val source = Source.fromFile(filename)

trait ShapeType extends Comparable[ShapeType] {
  val value: String

  def score: Int = {
    value match {
      case "A" => 1
      case "B" => 2
      case "C" => 3
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

trait RockTrait extends ShapeType
trait PaperTrait extends ShapeType
trait ScissorTrait extends ShapeType

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
    case "A" => Rock(value)
    case "B" => Paper(value)
    case "C" => Scissor(value)
    case "X" => Rock(value)
    case "Y" => Paper(value)
    case "Z" => Scissor(value)
    case _ => throw new IllegalArgumentException(s"Unknown value $value")
  }
}

val scores = mutable.ListBuffer.empty[Int]
for (line <- source.getLines) {
  line match {
    case s"$left $right" =>
      val them = ShapeType(left)
      val mine = ShapeType(right)
      val score = mine.scoreAgainst(them)
      scores.addOne(score)
      println(s"${mine.describeScore(them)}: $them vs $mine")
    case _ =>
      println(s"Unable to parse $line")
  }
}
source.close()

println(s"Total scores: ${scores.sum}")
