package aoc

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success
import scala.util.Failure

@main def aoc(testNos: String*): Unit = 
  val AllExercises = IndexedSeq(Ex01, Ex02, Ex03, Ex04, Ex05, Ex06, Ex06b, Ex07, Ex08)
  val loops = 25  // To measure jitted performance.

  val validTestNos = testNos.flatMap(_.toIntOption).filter(_ <= AllExercises.length)

  val exercises = 
    if validTestNos.isEmpty then AllExercises
    else validTestNos.map(x => AllExercises(x - 1))

  def exRuns = 
    Future.sequence {
      exercises.map { ex =>
        Future {
          val runs = for i <- 1 to loops yield ex.run
          runs.last
        }
      }
    }.andThen {
      case Success(results) =>
        for Result(name, p1, p1t, p2, p2t, parseTime, commonTime, totalTime) <- results do 
          val p1time = parseTime + commonTime + p1t
          val p2time = parseTime + commonTime + p2t
          val parseStr = f"parse: $parseTime%.1f ms"
          val commonStr = if commonTime > 0.1 then f", common: $commonTime%.1f ms" else ""

          println(f"""$name%-5s => part 1: "$p1", $p1time%.1f ms; part 2: "$p2", $p2time%.1f ms; """ +
                  parseStr + commonStr +  f", total: $totalTime%.1f ms")
      case Failure(e) => throw e
    }

  Await.ready(exRuns, 10 minutes)


case class Result(name: String, part1: Any, p1Time: Double, part2: Any, p2Time: Double, parseTime: Double, commonTime: Double, totalTime: Double)

trait Exercise:
  type ParsedInput
  type Common
  val name = getClass.getSimpleName.nn.init
  val num = name.drop(2)
  def input = scala.io.Source.fromFile(s"input/input_${num}.txt").getLines

  def parseInput(input: Iterator[String]): ParsedInput
  def common(input: ParsedInput): Common
  def part1(commonRes: Common): Any
  def part2(commonRes: Common): Any

  def run: Result = run(input)
  def run(input: Iterator[String]): Result =
    measure {
      val (parsedInput, parseTime) = measure { parseInput(input) }
      val (commonRes, commonTime) = measure { common(parsedInput) }
      (measure { part1(commonRes) },  measure { part2(commonRes) }, parseTime, commonTime)
    } match
      case (((pt1, pt1Time), (pt2, pt2Time), parseTime, commonTime), totalTime) =>
        Result(name,
               pt1, pt1Time, pt2, pt2Time, parseTime, commonTime, totalTime)
  
