package planet7.relational.csv

import planet7.relational.csv.Fields.{EmptyField, Field}

case class Csv(data: String) {
  private val allRows = data.trim.split("\n").toList
  private val header = data(allRows.head)
  private def data(s: String) = s.split(",").toList

  def rows: List[Row] = allRows.tail.filter(_.trim.nonEmpty).map(row => Row(header zip data(row)))
}

object Rows {
  val emptyRow = Row(Nil)
}

case class Row(values: List[(String, String)]) extends Diffable[Row, Field] {
  val sortedData: List[Field] = values.sortBy(_._1)
  def value(key: String): Option[String] = values.find(_._1 == key).map(_._2)
  override def toString = values.map(_._2).mkString("[", ", ", "]")

  def zero = EmptyField

  def sorted(key: Field => String) = values.sortBy(_._1)
}

object Fields {
  type Field = (String, String)

  object EmptyField extends Field("", "")

  implicit class DiffableField(f: Field) extends Field(f._1, f._2) with Diffable[Field, String] {
    def zero = ""
    def sorted(key: String => String) = ??? // No point in sorting a Field
  }
}


trait Diffable[T, U] {
  def zero: U
  def sorted(key: U => String): List[U]
}