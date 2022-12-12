package aoc

import scala.collection.mutable
import fastparse._, MultiLineWhitespace._

object Ex11 extends Exercise:
  type ParsedInput = Seq[Monkey]
  override type Common = Seq[Monkey]
  case class Move(monkeyId: Int, worry: Long)
  class Monkey(val id: Int, val initItems: Seq[Int], val divisor: Int, val throwItem: (Long, Long, Long) => Move):
    val itemWorries: mutable.Queue[Long] = mutable.Queue(initItems.map(_.toLong): _*)
    var inspected: Int = 0
    override def toString = s"Monkey $id: ${itemWorries.toSeq.mkString(",")}"

  object Monkey:
    def number[p: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def intro[p: P] = P("Monkey " ~/ number ~ ":")
    def startingItems[p: P] = P("Starting items: " ~/ number.rep(sep=","))
    def operation[p: P] = P("Operation: new = old " ~/ CharIn("+*").! ~ ("old".! | number))
    def divisble[p: P] = P("Test: divisible by " ~/ number)
    def ifTrue[p: P] = P("If true: throw to monkey " ~/ number)
    def ifFalse[p: P] = P("If false: throw to monkey " ~/ number)

    def monkey[p: P]: P[Monkey] = P(intro ~/ startingItems ~/ operation ~/ divisble ~/ ifTrue ~/ ifFalse).map {
      (id, items, op, divisor, ifTrue, ifFalse) => 
        val fn = (old: Long, boredem: Long, modulus: Long) =>
          val worryLevel = 
            ((op match
              case ("+", x:Int) => old + x
              case ("*", x:Int) => old * x
              case ("*", "old") => ((BigInt(old) * BigInt(old)) % modulus).toLong
            ) / boredem) % modulus
          val nextMonkey = if worryLevel % divisor == 0 then ifTrue else ifFalse
          Move(nextMonkey, worryLevel)

        Monkey(id.toInt, items, divisor, fn)
    }
    def monkeys[p: P]: P[Seq[Monkey]] = P(monkey.rep(sep = "") ~ End)

  def parseInput(input: Iterator[String]) = 
    val res = parse(input.mkString, Monkey.monkeys(_))
    val monkeys = res.get.value
    monkeys

  def common(input: ParsedInput) = input

  def part1(monkeySeq: Seq[Monkey]) =
    val monkeys = monkeySeq.map(m => Monkey(m.id, m.initItems, m.divisor, m.throwItem))
    var modulus = (for m <- monkeys yield m.divisor.toLong).product

    for round <- 1 to 20 do
      for monkey <- monkeys do
        while monkey.itemWorries.nonEmpty do
          monkey.inspected += 1
          val Move(id, worry) = monkey.throwItem(monkey.itemWorries.dequeue(), 3L, modulus)
          monkeys(id).itemWorries += (worry)
      
    monkeys.map(_.inspected).sorted.takeRight(2).map(_.toLong).product

  def part2(monkeySeq: Seq[Monkey]) =
    val monkeys = monkeySeq.map(m => Monkey(m.id, m.initItems, m.divisor, m.throwItem))
    var modulus = (for m <- monkeys yield m.divisor.toLong).product

    for round <- 1 to 10_000 do
      for monkey <- monkeys do
        while monkey.itemWorries.nonEmpty do
          monkey.inspected += 1
          val Move(id, worry) = monkey.throwItem(monkey.itemWorries.dequeue(), 1L, modulus)
          monkeys(id).itemWorries += (worry)

    monkeys.map(_.inspected).sorted.takeRight(2).map(_.toLong).product
    