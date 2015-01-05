package planet7.tabular

case class Row(data: Array[String], validationFailures: Seq[String] = Nil) {
  override def toString = data.mkString(",")

  def isInvalid = validationFailures != Nil && validationFailures.nonEmpty

  override def canEqual(that: Any) = that.isInstanceOf[Row]

  override def equals(that: Any) = that match {
    case thatRow: Row => this.data.deep == thatRow.data.deep
    case _ => false
  }
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