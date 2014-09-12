package planet7.tabular

trait Differentiator[U,K] {
  def zero: U

  /** Diff requires that there exists an Ordering for its keys */
  def key(u: U)(implicit evidence: Ordering[K]): K
}

trait SortingDifferentiator[U,K] extends Differentiator[U,K] {
  def sort(it: Iterator[U])(implicit evidence: Ordering[K]): Iterator[U] = it.toSeq.sortBy(key).iterator
}