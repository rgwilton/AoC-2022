package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex07Test extends ExTest(Ex07):
  val testcases = Seq(
    TestCase(
 """|$ cd /
    |$ ls
    |dir a
    |14848514 b.txt
    |8504156 c.dat
    |dir d
    |$ cd a
    |$ ls
    |dir e
    |29116 f
    |2557 g
    |62596 h.lst
    |$ cd e
    |$ ls
    |584 i
    |$ cd ..
    |$ cd ..
    |$ cd d
    |$ ls
    |4060174 j
    |8033020 d.log
    |5626152 d.ext
    |7214296 k""", 95437, 24933642),
    TestCase(exInput, 1118405, 12545514)
)