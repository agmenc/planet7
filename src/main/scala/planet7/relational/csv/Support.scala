package planet7.relational.csv

import planet7.relational.Differentiator
import planet7.relational.csv.RowSupport._

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

object RowSupport {

  case class Row(values: List[(String, String)])  {
    val sortedData = values.sortBy(_._1)
    def value(fieldName: String): Option[String] = values.find(_._1 == fieldName).map(_._2)
    override def toString = values.map(_._2).mkString("[", ", ", "]")
  }
  
  object EmptyRow extends Row(Nil)
  
  case class RowDiffer(fieldsInKey: String*) extends Differentiator[Row] {
    def zero = EmptyRow
    def key(u: Row) = fieldsInKey.map(u.value).mkString
  }
}

object FieldSupport {
  type Field = (String, String)

  object EmptyField extends Field("", "")

  case object FieldDiffer extends Differentiator[Field] {
    def zero = EmptyField
    def key(u: Field) = u._1 // sort fields by field name
  }
}