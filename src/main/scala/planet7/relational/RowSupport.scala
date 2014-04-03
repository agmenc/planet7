package planet7.relational

object RowSupport {
  case class Row(values: List[(String, String)])  {
    def value(fieldName: String): String = values find(_._1 == fieldName) map(_._2) getOrElse ""
    private[relational] def keepColumns(names: String*): Row = Row(values filter(t => names.contains(t._1)))
    private[relational] def columnNames = values map (v => v._1)
    private[relational] def columnValues = values map (v => v._2)
    override def toString = values map(_._2) mkString("[", ", ", "]")
  }

  object EmptyRow extends Row(Nil)

  case class RowDiffer(fieldsInKey: String*) extends Differentiator[Row] {
    def zero = EmptyRow
    def key(u: Row) = fieldsInKey.map(u.value).mkString
  }
}