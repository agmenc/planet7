package planet7.tabular

import java.io._
import java.nio.file.Paths
import java.util.Scanner

import scala.collection.AbstractTraversable

trait TabularDataSource extends Closeable {
  def header: Row

  // TODO - CAS - 12/08/2014 - Change to iterator because Kevin says so
  def rows: Iterator[Row]
}

class ScannerDataSource(file: File) extends TabularDataSource {
  val scanner = new Scanner(Paths.get(file.toURI))
  val header = if (scanner.hasNext) toRow(scanner.nextLine()) else throw new EmptyFileException

  override def rows = new Iterator[Row] {
    var line = scanner.nextLine()

    override def hasNext = line != null

    override def next() = {
      val oldLine = line
      line = scanner.nextLine()
      if (line == null) scanner.close()
      toRow(oldLine)
    }
  }

  override def close() = scanner.close()
}

class EmptyFileException extends RuntimeException("Empty file")