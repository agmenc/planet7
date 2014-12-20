package planet7.tabular

import java.io._
import java.nio.file.Paths
import java.util.Scanner

/**
 * A way of providing data to an instance of the Csv class. Most needs should be met by
 * BufferedDataSource, which has a number of implicit adapters declared in the planet7
 * package.
 */
trait TabularDataSource extends Closeable {
  /**
   * @return The header row containing the column names. If there is no header,
   *         implementors may wish to throw NoDataInSourceException
   */
  def header: Row

  /**
   * @return The rows of data provided by this data source. Implementors should consider
   *         closing the underlying datasource when the Iterator is exhausted.
   */
  def rows: Iterator[Row]
}

class NoDataInSourceException(sourceDescription: String) extends RuntimeException(sourceDescription)

class ScannerDataSource(file: File, parser: Parser) extends TabularDataSource {
  private val scanner = new Scanner(Paths.get(file.toURI))
  override val header = if (scanner.hasNext) parser.read(scanner.nextLine()) else throw new NoDataInSourceException(file.getCanonicalPath)

  override def rows = new Iterator[Row] {
    override def hasNext = scanner.hasNext
    override def next() = if (scanner.hasNext) parser.read(scanner.nextLine()) else null
  }

  override def close() = scanner.close()
}