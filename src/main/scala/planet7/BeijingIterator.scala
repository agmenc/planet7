package planet7

object BeijingIterator {
  implicit def fromIterable[U](itb: Iterable[U]): BeijingIterator[U] = new BeijingIterator[U](itb.iterator)
  implicit def fromIterator[U](it: Iterator[U]): BeijingIterator[U] = new BeijingIterator[U](it)
}

class BeijingIterator[U](it: Iterator[U]) extends Iterator[U] {
  private var first: Option[U] = if (it.hasNext) Some(it.next()) else None

  override def hasNext = first.isDefined
  override def next() = {
    val r = first.get
    first = if (it.hasNext) Some(it.next()) else None
    r
  }

  def headOption: Option[U] = first
  def head: U = first.get
  def tail: BeijingIterator[U] = new BeijingIterator[U](it)
}