package aoc

object Ex04 extends Exercise:
 type ParsedInput = Iterator[(Range, Range)]
 override type Common = Array[(Range, Range)]

  val Line = """(\d+)-(\d+),(\d+)-(\d+)""".r
  def parseInput(input: Iterator[String]) = 
    for case Line(r1l, r1h, r2l, r2h) <- input yield
      (r1l.toInt to r1h.toInt, r2l.toInt to r2h.toInt)

  def common(parsedInput: ParsedInput) = parsedInput.toArray

  extension (range: Range)
    def includes(r2: Range) = r2.start >= range.start && r2.end <= range.end
    def overlaps(r2: Range) = (r2.start <= range.end && r2.end >= range.start) ||
                              (range.start <= r2.end && range.end >= r2.start)
  def part1(ranges: Common) = 
    ranges
    .filter { (r1, r2) => (r1 includes r2) || (r2 includes r1) }
    .length

  def part2(ranges: Common) =
    ranges
    .filter { (r1, r2) => r1 overlaps r2 }
    .length
    