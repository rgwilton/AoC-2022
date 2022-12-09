package aoc

import scala.collection.mutable

object Ex07 extends Exercise:
  type ParsedInput = FileSystem
  type Common = mutable.Map[Path, Int]
  type Path = List[String]
  type FileSize = Int
  type DirContents = Set[String]
  type FileSystem = mutable.Map[Path, FileSize | DirContents]

  def parseInput(input: Iterator[String]) =
    val CdR = """[$] cd ([a-z./]+)""".r
    val DirR = """dir ([a-z.]+)""".r
    val FileR = """(\d+) ([a-z.]+)""".r
    var curPath: Path = List()

    val fs: FileSystem = mutable.Map()
    def updateFsDirectory(path: Path, fileOrDirName: String) =
      val curDir = fs.getOrElse(path, Set()).asInstanceOf[DirContents]
      fs += path -> (curDir + fileOrDirName)

    for line <- input do
      line match
        case FileR(size, filename) =>
          updateFsDirectory(curPath, filename)
          fs += (filename :: curPath) -> size.toInt

        case DirR(dirName) => 
          updateFsDirectory(curPath, dirName)

        case CdR(path) =>
          path match 
            case ".." => curPath = curPath.tail
            case "/" => curPath = List()
            case dirname => curPath = dirname :: curPath

        case _ => // Ignore the ls command.
    fs

  def common(fs: FileSystem) =
    // Build a map from dir path to size.  Root directory is listed with an empty path.
    val dirSizes = mutable.Map[Path, Int]()
    def sizeOf(p: Path): Int =
      fs(p) match
        case d:DirContents =>
          val dirSize = d.map(dirName => sizeOf(dirName :: p)).sum
          dirSizes += p -> dirSize
          dirSize
        case f:FileSize => f

    fs.keysIterator.foreach(sizeOf)
    dirSizes

  def part1(dirSizes: mutable.Map[Path, Int]) =
    dirSizes.values.filter(_ <= 100000).sum
    
  def part2(dirSizes: mutable.Map[Path, Int]) =
    def rootSize = dirSizes(List())
    val reductionNeeded = rootSize - (70000000 - 30000000)
    dirSizes.values.filter(_ > reductionNeeded).min
