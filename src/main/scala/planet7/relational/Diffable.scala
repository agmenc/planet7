package planet7.relational

trait Diffable[U] {
  def zero: U
  def sorted(key: Option[U] => String): List[U]
}