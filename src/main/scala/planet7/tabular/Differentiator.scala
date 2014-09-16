package planet7.tabular

trait Differentiator[U,K] {
  def zero: U
  def key(u: U)(implicit requiredForDiff: Ordering[K]): K
}