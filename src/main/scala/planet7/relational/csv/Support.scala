package planet7.relational.csv

import planet7.relational.csv.FieldSupport._
import planet7.relational.csv.RowSupport._
import planet7.relational.Diffable

object CsvSupport {
  // Are the below separate concerns? Pre-validations outside of the diff?
  // Validate that the key exists in both CSVs
  // Extract duplicates (or identify keys with dupe values)

  def key(maybeRow: Option[Row]): String = maybeRow.fold("")(row => row.value("ID").fold("")(s => s))

  case class Csv(data: String) extends Diffable[Row] {
    private val allRows = data.trim.split("\n").toList
    private val header = toRowValues(allRows.head)
    private def toRowValues(s: String) = s.split(",").toList

    def zero = RowSupport.emptyRow
    def sorted(key: (Option[Row]) => String) = allRows.tail
      .filter(_.trim.nonEmpty)
      .map(line => Row(header zip toRowValues(line)))
      .sortBy(row => key(Some(row)))
  }
}

object RowSupport {
  val emptyRow = Row(Nil)
  def key(maybeField: Option[Field]): String = maybeField.fold("")(_._1)

  case class Row(values: List[(String, String)]) extends Diffable[Field] {
    val sortedData: List[Field] = values.sortBy(_._1)
    def value(key: String): Option[String] = values.find(_._1 == key).map(_._2)
    override def toString = values.map(_._2).mkString("[", ", ", "]")

    def zero = EmptyField
    def sorted(key: Option[Field] => String) = values.sortBy(_._1)
  }
}

object FieldSupport {
  type Field = (String, String)

  object EmptyField extends Field("", "")

  implicit class DiffableField(f: Field) extends Field(f._1, f._2) with Diffable[String] {
    def zero = ""
    def sorted(key: Option[String] => String) = ??? // No point in sorting a Field
  }
}