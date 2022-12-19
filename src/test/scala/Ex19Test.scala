package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex19Test extends ExTest(Ex19):
  val testcases = Seq(
    TestCase(
   """|Blueprint 1:
      |  Each ore robot costs 4 ore.
      |  Each clay robot costs 2 ore.
      |  Each obsidian robot costs 3 ore and 14 clay.
      |  Each geode robot costs 2 ore and 7 obsidian.
      |
      |Blueprint 2:
      |  Each ore robot costs 2 ore.
      |  Each clay robot costs 3 ore.
      |  Each obsidian robot costs 3 ore and 8 clay.
      |  Each geode robot costs 3 ore and 12 obsidian.""", 33, 3472),
    TestCase(exInput, 1389, 3003))
