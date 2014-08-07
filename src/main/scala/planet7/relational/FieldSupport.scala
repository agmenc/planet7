package planet7.relational

trait FieldSupport {
  type Field = (String, String)
  type FieldDiff = (Field, Field)

  object EmptyField extends Field("", "")

  case object FieldDiffer extends Differentiator[Field] {
    def zero = EmptyField
    def key(u: Field) = u._1 // sort fields by field name
  }

  def prettyPrint(fieldDiffs: Seq[FieldDiff]): Seq[String] = fieldDiffs.map {
    case (left, right) => s"${left._1}: ${left._2} -> ${right._2}"
  }
}