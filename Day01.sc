import scala.collection.mutable
import scala.io.Source

val filename = "/Users/hendra.saputra/dev/bolo/aoc2022/Day01.txt"
val elvesCalories = mutable.ListBuffer.empty[Int]
val source = Source.fromFile(filename)
var calories = 0

for (line <- source.getLines) {
  if (line.nonEmpty) {
    calories += line.toInt
  }
  else {
    elvesCalories.addOne(calories)
    calories = 0
  }
}
source.close()

if (calories > 0) elvesCalories.addOne(calories)
val sortedCalories = elvesCalories.sorted(Ordering.Int.reverse)

val mostCalories = sortedCalories.head
println(s"Day #1 - Most calories: $mostCalories")

val topThreeCalories = sortedCalories.take(3)
println(s"Day #1 - Top 3 calories are: ${topThreeCalories.mkString(", ")}. With total: ${topThreeCalories.sum}")
