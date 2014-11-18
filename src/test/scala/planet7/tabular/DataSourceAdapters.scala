package planet7.tabular

import com.github.tototoshi.csv.CSVReader

object DataSourceAdapters {
  implicit def adapterForExternalParser(reader: CSVReader): TabularDataSource = new TabularDataSource {
    override val header = reader.readNext().fold(throw new NoDataInSourceException(reader.toString))(data => Row(data.toArray))
    override def rows = reader.iterator.map(items => Row(items.toArray))
    override def close() = reader.close()
  }
}