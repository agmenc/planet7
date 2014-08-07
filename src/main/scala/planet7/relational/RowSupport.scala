package planet7.relational

trait RowSupport {
  case class Row(values: Seq[(String, String)])  {
    def value(fieldName: String): String = field(fieldName).fold("")(_._2)
    private def field(fieldName: String): Option[(String, String)] = values find (x => x._1 == fieldName)

    def rename(deltas: Map[String, String]): Row = Row(values map (f => renameHeader(deltas, f._1) -> f._2))
    private def renameHeader(deltas: Map[String,String], header: String) = deltas.getOrElse(header, header)
    
    def restructure(names: String*): Row = Row(names map (name => field(name).fold(name -> "")(identity)))

    def replace(mappings: Map[String, String => String]): Row = Row(values map replaceWith(mappings))

    def and(predicates: (String, String => Boolean)*): Boolean = predicates.forall(pred => pred._2(value(pred._1)))

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

  object RowTransforms {
    def rename(nameChanges: (String,String)*): Row => Row = row => row.rename(Map(nameChanges:_*))
    def restructure(columnNames: String*): Row => Row = row => row.restructure(columnNames:_*)
  }

  object RowPredicates {
    def and(predicates: (String, String => Boolean)*): Row => Boolean = row => row.and(predicates:_*)
  }
}