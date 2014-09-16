package planet7

import planet7.tabular.Differentiator

import scala.annotation.tailrec

/** The result of a diff is a Seq of differences, each of which is also (potentially) diffable */
object Diff {
  def apply[U,K: Ordering](lefts: Iterable[U], rights: Iterable[U], differ: Differentiator[U,K]): Seq[(U, U)] = apply(lefts.iterator, rights.iterator, differ)

  def apply[U,K: Ordering](lefts: Iterator[U], rights: Iterator[U], differ: Differentiator[U,K]): Seq[(U, U)] = {
    import scala.math.Ordered.orderingToOrdered

    @tailrec
    def eliminateIdenticalElements(left: BeijingIterator[U], right: BeijingIterator[U], key: U => K)(diffs: Seq[(U, U)]): Seq[(U, U)] =
      (left.headOption, right.headOption) match {
        case (None, None) => diffs
        case (None, Some(_)) => right.map((elem: U) => differ.zero -> elem).toSeq ++ diffs
        case (Some(_), None) => left.map((elem: U) => elem -> differ.zero).toSeq ++ diffs
        case (Some(sl), Some(sr)) => (key(sl), key(sr)) match {
          case (l, r) if l > r => eliminateIdenticalElements(left, right.tail, key)((differ.zero, right.head) +: diffs)
          case (l, r) if l < r => eliminateIdenticalElements(left.tail, right, key)((left.head, differ.zero) +: diffs)
          case (l, r) => eliminateIdenticalElements(left.tail, right.tail, key)(if (left.head == right.head) diffs else (left.head, right.head) +: diffs)
        }
      }

    eliminateIdenticalElements(lefts, rights, differ.key)(Nil)
  }
}

/** We sort left and right inputs by a key, and so the results follow the same order */
object PreSortingDiff {
  def apply[U,K: Ordering](lefts: Iterable[U], rights: Iterable[U], differ: Differentiator[U,K]): Seq[(U, U)] = apply(lefts.iterator, rights.iterator, differ)

  def apply[U,K: Ordering](lefts: Iterator[U], rights: Iterator[U], differ: Differentiator[U,K]): Seq[(U, U)] = {
    def sort(it: Iterator[U]): Seq[U] = it.toSeq.sortBy(differ.key)

    Diff(sort(lefts), sort(rights), differ)
  }
}