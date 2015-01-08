package planet7.tabular.datasources

import com.github.tototoshi.csv.CSVReader
import planet7.tabular.{NoDataInSourceException, Row, TabularDataSource}

object CsvReaderDataSource {
  implicit def fromCsvReader(reader: CSVReader): TabularDataSource = new TabularDataSource {
    override val header = reader.readNext().fold(throw new NoDataInSourceException(reader.toString))(data => Row(data.toArray))
    override def rows = reader.iterator.map(items => Row(items.toArray))
    override def close() = reader.close()
  }
}