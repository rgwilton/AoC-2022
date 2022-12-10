package aoc

import scala.collection.mutable
import scala.annotation.tailrec

object Ex10 extends Exercise:
  type ParsedInput = Seq[String]
  override type Common = Seq[Int]
  
  case object Addx:
    def unapply(s: String): Option[Int] =
      if s.startsWith("addx") then Some((s.drop(5).toInt)) else None

  def parseInput(input: Iterator[String]) = input.toSeq

  def common(input: ParsedInput) =
    var reg = 1
    reg +:
    input
    .flatMap {
      case "noop" => Seq(reg)
      case Addx(x) => 
        val oldReg = reg
        reg = reg + x
        Seq(oldReg, reg)
    }

  def part1(regValues: Seq[Int]) =
    regValues
    .iterator
    .zip(Iterator.from(1))
    .collect {
      case (r, idx) if ((idx - 20) % 40 == 0) => r * idx
    }
    .take(6)
    .sum

  def part2(regValues: Seq[Int]) =
    val display = 
      regValues
      .iterator
      .zip(Iterator.from(0).map(_ % 40))
      .collect {
        case (r, idx) =>
          if idx >= r - 1 && idx <= r + 1 then '#' else '.'
      }.grouped(40)
      .map(_.mkString(""))
      .mkString("\n")

    //println(display)
    0