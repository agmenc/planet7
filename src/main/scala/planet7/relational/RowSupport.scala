package planet7.relational

object RowSupport {
  case class Row(values: List[(String, String)])  {
    def value(fieldName: String): String = values find(_._1 == fieldName) map(_._2) getOrElse ""
    def keepColumns(names: String*): Row = Row(values filter(f => names.contains(f._1)) sortBy(f => names.indexOf(f._1)))
    def replace(fieldName: String, mapping: Map[String, String]): Row = Row(values map(field => if (field._1 == fieldName) (field._1, mapping(field._2)) else field))
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