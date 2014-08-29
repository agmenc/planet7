package planet7.tabular

case class RowDiffer(indicesOfKey: Int*) extends Differentiator[Row] {
  def zero = EmptyRow
  def key(u: Row) = indicesOfKey.map(u.data).mkString
}

object EmptyRow extends Row(Array.empty[String])

case object FieldDiffer extends Differentiator[(String, String)] {
  override def zero = ("", "")
  override def key(u: (String, String)) = u._1

  def prettyPrint(fieldDiffs: Seq[((String, String), (String, String))]): Seq[String] = fieldDiffs.map {
    case (left, right) => s"${left._1}: ${left._2} -> ${right._2}"
  }
}