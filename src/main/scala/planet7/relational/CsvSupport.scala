package planet7.relational

import RowSupport.Row

object CsvSupport {
  case class Csv(headers: List[String], data: List[List[String]]) {
    def rows: List[Row] = data map(headers zip _) map Row
  }

  object Csv {
    def apply(data: String): Csv = toCsv(data.trim.split("\n").toList)

    private def toCsv(allRows: List[String]) = Csv(toRowValues(allRows.head), allRows.tail.filter(_.trim.nonEmpty).map(toRowValues))
    private def toRowValues(s: String) = s.split(",").toList
  }
}