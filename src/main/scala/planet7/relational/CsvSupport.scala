package planet7.relational

import RowSupport.Row
import scala.io.BufferedSource

object CsvSupport {
  case class Csv(headers: List[String], data: List[List[String]]) {
    def rows: List[Row] = data map(headers zip _) map Row
    def withMappings(mappings: (String, Map[String, String])*): Csv = Csv(rows map (_.replace(Map(mappings:_*))))
    def keepColumns(columns: String*): Csv = Csv(rows map (_.keepColumns(columns:_*)))

    def renameColumns(nameChanges: (String,String)*): Csv = renameColumns(Map(nameChanges:_*))
    private def renameColumns(deltas: Map[String,String]): Csv = Csv(headers map (renameHeader(deltas, _)), data)
    private def renameHeader(deltas: Map[String,String], header: String) = if (deltas.contains(header)) deltas(header) else header
  }

  object Csv {
    def apply(data: String): Csv = toCsv(data.trim.split("\n").toList)
    def apply(rows: List[Row]): Csv = Csv(rows.head.columnNames, rows.map(_.columnValues))
    def apply(dataSource: BufferedSource): Csv = Csv(dataSource.getLines().mkString("\n"))
    def apply(csvs: Csv*): Csv = apply(csvs)
    def apply(csvs: Iterable[Csv]): Csv = Csv(csvs.head.headers, csvs.flatMap(_.data)(collection.breakOut): List[List[String]])

    private def toCsv(allRows: List[String]) = Csv(toRowValues(allRows.head), allRows.tail filter(_.trim.nonEmpty) map toRowValues)
    private def toRowValues(s: String) = s.split(",").toList
  }
}