package planet7.tabular

import scala.collection.immutable.IndexedSeq

case class Row(data: Array[String], validationFailures: Seq[String] = Nil) extends Iterable[String] {
  override def toString = data.mkString(",")

  def isInvalid = validationFailures != Nil && validationFailures.nonEmpty

  def apply(index: Int): String = this.data(index)

  // TODO - CAS - 20/02/15 - Given the header, we should be able to extract a column by name
//  def apply(header: Row, column: String): String = this.data(index)

  override def canEqual(that: Any) = that.isInstanceOf[Row]

  override def equals(that: Any) = that match {
    case thatRow: Row => this.data.deep == thatRow.data.deep
    case _ => false
  }

  override def iterator = data.iterator
}

object Row {
  def indexOf(header: Row, column: String): Int = header.data.indexOf(column) match {
    case notFound if notFound < 0 => throw new ColumnDoesNotExistException(column, header)
    case ok => ok
  }
}

class ColumnDoesNotExistException(columnName: String, header: Row) extends RuntimeException {
  override def getMessage = s"Cannot find column '$columnName' in header with columns:\n${header.data.mkString("\n")}\n"
}