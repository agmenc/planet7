package planet7.relational

object RowSupport {
  case class Row(values: List[(String, String)])  {
    val sortedData = values.sortBy(_._1)
    def value(fieldName: String): String = values.find(_._1 == fieldName).map(_._2).getOrElse("")
    override def toString = values.map(_._2).mkString("[", ", ", "]")
  }

  object EmptyRow extends Row(Nil)

  case class RowDiffer(fieldsInKey: String*) extends Differentiator[Row] {
    def zero = EmptyRow
    def key(u: Row) = fieldsInKey.map(u.value).mkString
  }
}