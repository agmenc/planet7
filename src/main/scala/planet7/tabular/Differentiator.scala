package planet7.tabular

trait Differentiator[U] {
  def zero: U
  def ordering: Ordering[U]
}