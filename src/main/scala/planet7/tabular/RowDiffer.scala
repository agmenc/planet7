package planet7.tabular

case class RowDiffer(indicesOfKey: Int*) extends Differentiator[Row,String] {
  override def zero = EmptyRow
  override def key(row: Row)(implicit evidence: Ordering[String]) = indicesOfKey.map(row.data).mkString
}

object RowDiffer {
  def apply(header: Row, indexColumns: (String,String => Any)*): RowDiffer = {
    val indices = indexColumns map { case (colName, fn) => header.data.indexOf(colName)}
    new RowDiffer(indices:_*)
  }
}

// TODO - CAS - 16/09/2014 - remove - redundant
case class NonSortingRowDiffer(indicesOfKey: Int*) extends Differentiator[Row,String] {
  override def zero = EmptyRow
  override def key(row: Row)(implicit evidence: Ordering[String]) = if (row.data.isEmpty) "" else indicesOfKey.map(row.data).mkString
}

object EmptyRow extends Row(Array.empty[String])

case object FieldDiffer extends Differentiator[(String, String), String] {
  override def zero = ("", "")
  override def key(u: (String, String))(implicit evidence: Ordering[String]) = u._1

  def prettyPrint(fieldDiffs: Seq[((String, String), (String, String))]): Seq[String] = fieldDiffs.map {
    case (left, right) => s"${left._1}: ${left._2} -> ${right._2}"
  }
}