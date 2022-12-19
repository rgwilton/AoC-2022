package aoc

import scala.collection.mutable
import fastparse._

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success
import scala.util.Failure

object Ex19 extends Exercise:
  type ParsedInput = Seq[Blueprint]
  override type Common = ParsedInput

  case class Blueprint(id: Int, oroc: Int, croc: Int, broc: Int, brcc: Int, groc: Int, grbc: Int):
    val maxOre = Seq(oroc, croc, broc, groc).max
    case class Typs(ore: Int, clay: Int, obsidian: Int, geode: Int)
    def calc(remTime: Int, robots: Typs, items: Typs): Int = {
      def calcTypes(ore: Int, clay: Int, obsidian: Int, geode: Int) =
        Typs(robots.ore + items.ore - ore,
             robots.clay + items.clay - clay,
             robots.obsidian + items.obsidian - obsidian,
             robots.geode + items.geode - geode)
      if remTime == 0 then
        items.geode
      else
        var geodes = items.geode
        if items.obsidian >= grbc && items.ore >= groc then
          val newItems = calcTypes(ore = groc, clay = 0, obsidian = grbc, geode = 0)
          val newRobots = robots.copy(geode = robots.geode + 1)
          geodes = calc(remTime - 1, newRobots, newItems) max geodes
        else
          if items.clay >= brcc && items.ore >= broc && robots.obsidian < grbc then
              val newItems = calcTypes(ore = broc, clay = brcc, obsidian = 0, geode = 0)
              val newRobots = robots.copy(obsidian = robots.obsidian + 1)
              geodes = calc(remTime - 1, newRobots, newItems) max geodes
          
          if robots.clay < brcc && items.clay < 2 * brcc && items.ore >= croc then
            val newItems = calcTypes(ore = croc, clay = 0, obsidian = 0, geode = 0)
            val newRobots = robots.copy(clay = robots.clay + 1)
            geodes = calc(remTime - 1, newRobots, newItems) max geodes

          if robots.ore < maxOre && items.ore >= oroc then
            val newItems = calcTypes(ore = oroc, clay = 0, obsidian = 0, geode = 0)
            val newRobots = robots.copy(ore = robots.ore + 1)
            geodes = calc(remTime - 1, newRobots, newItems) max geodes
          
          if items.ore < maxOre ||
              (robots.clay > 0 && items.clay < brcc) then
            val newItems = calcTypes(ore = 0, clay = 0, obsidian = 0, geode = 0)
            geodes = calc(remTime - 1, robots, newItems) max geodes

        geodes
    }
    def calc(mins: Int): (Int, Int) = (id, calc(mins, robots = Typs(1,0,0,0), items = Typs(0,0,0,0)))

  object Parser:
    import MultiLineWhitespace._
    def number[p: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def BlueprintP[p: P] =
       P("Blueprint" ~/ number ~/ ":" ~/
         "Each ore robot costs"~/ number ~/ "ore." ~/
         "Each clay robot costs" ~/ number ~/ "ore." ~/
         "Each obsidian robot costs" ~/ number ~/ "ore and" ~/ number ~/ "clay." ~/
         "Each geode robot costs" ~ number ~"ore and" ~/ number ~/ "obsidian.").map(Blueprint.apply)
    def inputP[p: P] = P(BlueprintP.rep(sep = "") ~ End)

  def parseInput(input: Iterator[String]) = 
    val res = parse(input.mkString("\n"), Parser.inputP(_))
    res.get.value

  def common(input: ParsedInput) = 
    //for b <- input do println(b)
    input 
 
  def part1(blueprints: Common) =
    val resultsF =
      Future.sequence {
        blueprints.map { blueprint =>
          Future { blueprint.calc(24) }
        }
      }
      
    val results = Await.result(resultsF, 1 minutes)
    //println(results)
    results.map((a,b) => a*b).sum

  def part2(blueprints: Common) = 
    val resultsF =
      Future.sequence {
        blueprints.take(3).map { blueprint =>
          Future { blueprint.calc(32) }
        }
      }

    val results = Await.result(resultsF, 1 minutes)
    //println(results)
    results.map((a,b) => b).product
