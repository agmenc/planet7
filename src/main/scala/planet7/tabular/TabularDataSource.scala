package planet7.tabular

import java.io._
import java.nio.file.Paths
import java.util.Scanner

trait TabularDataSource extends Closeable {
  def header: Row

  // TODO - CAS - 12/08/2014 - Change to iterator because Kevin says so
  def rows: Iterator[Row]
}

class ScannerDataSource(file: File) extends TabularDataSource {
  val scanner = new Scanner(Paths.get(file.toURI))
  val header = if (scanner.hasNext) toRow(scanner.nextLine()) else throw new EmptyFileException

  override def rows = new Iterator[Row] {
    override def hasNext = scanner.hasNext

    override def next() = if (scanner.hasNext) toRow(scanner.nextLine()) else null
  }

  override def close() = scanner.close()
}

class EmptyFileException extends RuntimeException("Empty file")