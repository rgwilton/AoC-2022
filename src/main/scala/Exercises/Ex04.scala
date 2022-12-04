package aoc

object Ex04 extends Exercise:
 type ParsedInput = Iterator[(Range, Range)]
 override type Common = Array[(Range, Range)]

  val Line = """(\d+)-(\d+),(\d+)-(\d+)""".r
  def parseInput(input: Iterator[String]) = 
    for case Line(r1l, r1h, r2l, r2h) <- input yield
      (r1l.toInt to r1h.toInt, r2l.toInt to r2h.toInt)

  def common(parsedInput: ParsedInput) = parsedInput.toArray

  def includes(r1: Range, r2: Range) = 
    (r2.start >= r1.start && r2.end <= r1.end) || (r1.start >= r2.start && r1.end <= r2.end)
    
  def overlaps(r1: Range, r2: Range) = 
    (r2.start <= r1.end && r2.end >= r1.start) || (r1.start <= r2.end && r1.end >= r2.start)

  def part1(ranges: Common) = ranges.count(includes)

  def part2(ranges: Common) = ranges.count(overlaps)