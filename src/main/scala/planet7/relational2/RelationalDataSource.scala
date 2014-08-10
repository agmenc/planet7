package planet7.relational2

import java.io._
import java.nio.file.Paths
import java.util.Scanner

import scala.collection.AbstractTraversable

trait RelationalDataSource {
  def header: Row
  def rows: Traversable[Row]
}

// 384 ms (vs 196 ms for the BufferedReader approach)
class ScannerDataSource(file: File) extends RelationalDataSource {
  val scanner = new Scanner(Paths.get(file.toURI))
  val header = if (scanner.hasNext) toRow(scanner.nextLine()) else throw new EmptyFileException

  override def rows = new AbstractTraversable[Row] {
    override def foreach[U](f: (Row) => U) = {
      while (scanner.hasNextLine) {
        val line = scanner.nextLine()
        if (line.nonEmpty) f(toRow(line))
      }
    }
  }
}

class EmptyFileException extends RuntimeException("Empty file")