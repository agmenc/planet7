package planet7.tabular

case class RowDiffer(indicesOfKey: Int*) extends SortingDifferentiator[Row] {
  def zero = EmptyRow
  def key(row: Row) = indicesOfKey.map(row.data).mkString
}

object RowDiffer {
  def apply(csv: Csv, columnNames: String*): RowDiffer = {
    val indices = columnNames map (cn => csv.header.data.indexOf(cn))
    new RowDiffer(indices:_*)
  }
}

object EmptyRow extends Row(Array.empty[String])

case object FieldDiffer extends SortingDifferentiator[(String, String)] {
  override def zero = ("", "")
  override def key(u: (String, String)) = u._1

  def prettyPrint(fieldDiffs: Seq[((String, String), (String, String))]): Seq[String] = fieldDiffs.map {
    case (left, right) => s"${left._1}: ${left._2} -> ${right._2}"
  }
}