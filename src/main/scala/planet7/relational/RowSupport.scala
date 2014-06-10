package planet7.relational

trait RowSupport {
  case class Row(values: List[(String, String)])  {
    def value(fieldName: String): String = field(fieldName).fold("")(_._2)
    private def field(fieldName: String): Option[(String, String)] = values find (x => x._1 == fieldName)

    def reorderColumns(names: String*): Row = Row(values sortBy(f => names.indexOf(f._1)))
    def retainColumns(names: String*): Row = Row(values filter(f => names.contains(f._1)))
    def reorderColumnsOrAdd(names: String*): Row = Row(names map (name => field(name).fold(name -> "")(identity)) toList)

    def replace(mappings: Map[String, String => String]): Row = Row(values map replaceWith(mappings))

    private def replaceWith(mappings: Map[String, String => String])(field: Field) =
      field._1 -> mappings.getOrElse(field._1, identity[String] _)(field._2)

    def columnNames = values map (v => v._1)

    def columnValues = values map (v => v._2)

    override def toString = values map(_._2) mkString ","
  }

  object EmptyRow extends Row(Nil)

  case class RowDiffer(fieldsInKey: String*) extends Differentiator[Row] {
    def zero = EmptyRow
    def key(u: Row) = fieldsInKey.map(u.value).mkString
  }
}