package planet7.tabular

trait Differentiator[U,K] {
  def zero: U
  def zeroKey: K
  def key(u: U): K // (implicit o: Ordering[K]): K // Something for which there is an ordering
  def keyOrEmpty(maybeU: Option[U]): K = maybeU.fold(zeroKey)(key)
  def sort(it: Iterator[U])(implicit o: Ordering[K]): Iterator[U] = it
}

trait SortingDifferentiator[U,K] extends Differentiator[U,K] {
  override def sort(it: Iterator[U])(implicit o: Ordering[K]): Iterator[U] = it.toSeq.sortBy(key).toIterator
}