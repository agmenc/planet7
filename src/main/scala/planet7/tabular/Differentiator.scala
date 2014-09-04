package planet7.tabular

trait Differentiator[U] {
  def zero: U
  def key(u: U): String
  def keyOrEmpty(maybeU: Option[U]): String = maybeU.fold("")(key)
  def sort(it: Iterator[U]): Iterator[U] = it
}

trait SortingDifferentiator[U] extends Differentiator[U] {
  override def sort(it: Iterator[U]): Iterator[U] = it.toSeq.sortBy(key).toIterator
}