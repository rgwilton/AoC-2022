package aoc

import scala.collection.mutable
import fastparse._

object Ex13 extends Exercise:
  type ParsedInput = Seq[PktPair]
  override type Common = Seq[PktPair]

  case object Lst:
    def apply(x: Lst | Int) = new Lst(List(x))
  case class Lst(elmts: List[ Lst | Int]) extends Ordered[Lst]:
    override def toString = elmts.mkString("[",",","]")

    def compare(that: Lst): Int =
      (this, that) match
        case (Lst((x:Int)::xs), Lst((y:Int)::ys)) => 
          val c = x compare y
          if c != 0 then c else Lst(xs) compare Lst(ys)
        case (Lst((x:Lst)::xs), Lst((y:Lst)::ys)) => 
          val c = x compare y
          if c != 0 then c else Lst(xs) compare Lst(ys)
        case (Lst((x:Int)::xs), b@Lst((y:Lst)::ys)) => Lst(Lst(List(x)):: xs) compare b
        case (a@Lst((x:Lst)::xs), Lst((y:Int)::ys)) => a compare Lst(Lst(List(y)):: ys) 
        case (Lst(x::xs), Lst(Nil)) => 1
        case (Lst(Nil), Lst(y::ys)) => -1
        case (Lst(Nil), Lst(Nil)) => 0

  type PktPair = (Lst, Lst)
  
  object Parser:
    import NoWhitespace._
    def number[p: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def listElem[p: P]: P[Lst|Int] = P(number | list)
    def list[p: P]: P[Lst] = P("[" ~/ listElem.rep(min = 0, sep = ",") ~ "]").map {
      elmts => Lst(elmts.toList)
    }
    def packet[p: P] = P(list)
    def packetPair[p: P] = P(list ~/ "\n" ~/ list)
    def inputP[p: P] = P(packetPair.rep(sep = "\n\n") ~ End)

  def parseInput(input: Iterator[String]) = 
    val res = parse(input.mkString("\n"), Parser.inputP(_))
    val pktPairs = res.get.value
    pktPairs

  def common(input: ParsedInput) = input

  def part1(packetPairs: Seq[PktPair]) =
    val res = 
      for (pkts, idx) <- packetPairs.zip(Iterator.from(1))
        if (pkts._1 <= pkts._2) yield idx
    res.sum

  def part2(packetPairs: Seq[PktPair]) =
    val pkts = packetPairs.toArray.flatMap {
      case (p1, p2) => Seq(p1, p2)
    }
    val d1 = Lst(Lst(2))
    val d2 = Lst(Lst(6))
    val allPkts = (pkts ++ Seq(d1, d2)).sorted
    val i1 = 1 + allPkts.indexOf(d1)
    val i2 = 1 + allPkts.indexOf(d2)
    i1 * i2

    