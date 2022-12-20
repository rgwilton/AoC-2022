package aoc

import scala.collection.mutable.Buffer

object Ex20 extends Exercise:
  type ParsedInput = IndexedSeq[Long]
  override type Common = ParsedInput
  class Link(val value: Long, var prev: Link, var next: Link)

  def parseInput(input: Iterator[String]) =
    input.map(_.toLong).toIndexedSeq

  def build(input: IndexedSeq[Long]) = 
    val len = input.length
    val links = input.map(x => Link(x, null, null))
    for i <- 1 to links.length - 2 do
      val l = links(i)
      l.prev = links(i - 1)
      l.next = links(i + 1)
    links(0).prev = links(len - 1)
    links(0).next = links(1)
    links(len - 1).prev = links(len - 2)
    links(len - 1).next = links(0)
    links

  def decrypt(links: IndexedSeq[Link]) = 
    def adjLen = links.length -1
    for i <- 0 until links.length do
      val l = links(i)

      // Minimize number of steps required.
      var steps = l.value % adjLen
      if steps > adjLen/2 then steps -= adjLen
      else if steps < -adjLen/2 then steps += adjLen

      if steps != 0 then
        // Unlink.
        l.prev.next = l.next
        l.next.prev = l.prev
        if (steps > 0) then
          var c = l
          for _ <- 1 to steps.toInt do c = c.next   
          // Insert link.
          val n = c.next
          c.next = l
          l.prev = c
          n.prev = l
          l.next = n
        if (steps < 0) then
          var c = l
          for _ <- 1 to -steps.toInt do c = c.prev   
          // Insert link.
          val p = c.prev
          c.prev = l
          l.next = c
          p.next = l
          l.prev = p

  def calcAnswer(links: IndexedSeq[Link]) =
    var l = links.find(_.value == 0).get
    val values = 
      for _ <- 1 to 3 yield
        for _ <- 1 to 1000 do l = l.next
        l.value
    values.sum

  def common(input: ParsedInput) = input

  def part1(input: IndexedSeq[Long]) =
    val links = build(input)
    decrypt(links)
    calcAnswer(links)

  def part2(input: Common) = 
    val links = build(input.map(_ * 811589153))
    for _ <- 1 to 10 do decrypt(links)
    calcAnswer(links)

