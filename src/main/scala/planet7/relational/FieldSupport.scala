package planet7.relational

object FieldSupport {
  type Field = (String, String)

  object EmptyField extends Field("", "")

  case object FieldDiffer extends Differentiator[Field] {
    def zero = EmptyField
    def key(u: Field) = u._1 // sort fields by field name
  }
}