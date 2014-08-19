package planet7.tabular

import java.io._
import java.nio.file.Paths
import java.util.Scanner

trait TabularDataSource extends Closeable {
  def header: Row
  def rows: Iterator[Row]
}

class ScannerDataSource(file: File) extends TabularDataSource {
  private val scanner = new Scanner(Paths.get(file.toURI))
  override val header = if (scanner.hasNext) toRow(scanner.nextLine()) else throw new NoDataInSourceException(file.getCanonicalPath)

  override def rows = new Iterator[Row] {
    override def hasNext = scanner.hasNext
    override def next() = if (scanner.hasNext) toRow(scanner.nextLine()) else null
  }

  override def close() = scanner.close()
}

class NoDataInSourceException(sourceDescription: String) extends RuntimeException(sourceDescription)