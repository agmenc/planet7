package planet7.tabular

trait Differentiator[U,K] {
  def zero: U
  def key(u: U)(implicit requiredForDiff: Ordering[K]): K
}

trait SortingDifferentiator[U,K] extends Differentiator[U,K] {
  def sort(it: Iterator[U])(implicit evidence: Ordering[K]): Iterator[U] = it.toSeq.sortBy(key).iterator
}