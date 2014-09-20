package planet7

import planet7.tabular.Differentiator

import scala.annotation.tailrec

// TODO - CAS - 19/09/2014 - Better CSV parsing, respecting quotes
// TODO - CAS - 20/09/2014 - Ability to set tolerances for numerical field comparisons
// TODO - CAS - 20/09/2014 - Identify duplicates in both lists

/** The result of a diff is a Seq of differences, each of which is also (potentially) diffable */
object NonSortingDiff {
  def apply[U](lefts: Iterable[U], rights: Iterable[U], differ: Differentiator[U]): Seq[(U, U)] = apply(lefts.iterator, rights.iterator, differ)

  def apply[U](lefts: Iterator[U], rights: Iterator[U], differ: Differentiator[U]): Seq[(U, U)] = {
    import scala.math.Ordered.orderingToOrdered

    implicit val orderingOnU = differ.ordering

    @tailrec
    def eliminateIdenticalElements(left: BeijingIterator[U], right: BeijingIterator[U], ordering: Ordering[U])(diffs: Seq[(U, U)]): Seq[(U, U)] =
      (left.headOption, right.headOption) match {
        case (None, None) => diffs
        case (None, Some(_)) => right.map((elem: U) => differ.zero -> elem).toSeq ++ diffs
        case (Some(_), None) => left.map((elem: U) => elem -> differ.zero).toSeq ++ diffs
        case (Some(sl), Some(sr)) => (sl, sr) match {
          case (l, r) if l > r => eliminateIdenticalElements(left, right.tail, ordering)((differ.zero, right.head) +: diffs)
          case (l, r) if l < r => eliminateIdenticalElements(left.tail, right, ordering)((left.head, differ.zero) +: diffs)
          case (l, r) => eliminateIdenticalElements(left.tail, right.tail, ordering)(if (left.head == right.head) diffs else (left.head, right.head) +: diffs)
        }
      }

    eliminateIdenticalElements(lefts, rights, differ.ordering)(Nil)
  }
}

/** We sort left and right inputs by a key, so they can be diffed */
object Diff {
  def apply[U](lefts: Iterable[U], rights: Iterable[U], differ: Differentiator[U]): Seq[(U, U)] = apply(lefts.iterator, rights.iterator, differ)

  def apply[U](lefts: Iterator[U], rights: Iterator[U], differ: Differentiator[U]): Seq[(U, U)] = {
    def sort(it: Iterator[U]): Seq[U] = it.toSeq.sorted(differ.ordering)

    NonSortingDiff(sort(lefts), sort(rights), differ)
  }
}