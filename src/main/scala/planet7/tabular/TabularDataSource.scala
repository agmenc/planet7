package planet7.tabular

import java.io._
import java.nio.file.Paths
import java.util.Scanner

import scala.collection.AbstractTraversable

trait TabularDataSource {
  def header: Row
  def rows(columnStructureTx: Row => Row): Traversable[Row]
}

// 384 ms (vs 196 ms for the BufferedReader approach)
class ScannerDataSource(file: File) extends TabularDataSource {
  val scanner = new Scanner(Paths.get(file.toURI))
  val header = if (scanner.hasNext) toRow(scanner.nextLine()) else throw new EmptyFileException

  override def rows(columnStructureTx: Row => Row) = new AbstractTraversable[Row] {
    override def foreach[U](f: (Row) => U) = {
      while (scanner.hasNextLine) {
        val line = scanner.nextLine()
        if (line.nonEmpty) f(columnStructureTx(toRow(line)))
      }
    }
  }
}

class EmptyFileException extends RuntimeException("Empty file")