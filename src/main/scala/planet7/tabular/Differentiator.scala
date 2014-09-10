package planet7.tabular

trait Differentiator[U,K] {
  def zero: U
  def zeroKey: K
  def key(u: U): K
  def sort(it: Iterator[U])(implicit o: Ordering[K]): Iterator[U] = it
}

trait SortingDifferentiator[U,K] extends Differentiator[U,K] {
  override def sort(it: Iterator[U])(implicit o: Ordering[K]): Iterator[U] = it.toSeq.sortBy(key).toIterator
}