package planet7

import planet7.tabular.Differentiator

import scala.annotation.tailrec

/**
 * Opinionated Diff:
 *  - We sort left and right inputs by a key, and so the results follow the same order
 *  - The result of a diff is a Seq of differences, each of which is also (potentially) diffable
 *
 *  // TODO - CAS - 12/08/2014 - Kev's suggestion: split out sorting from the primary concern of Diffing. After all, many datasets will be sorted correctly
 *  anyway. Mix in sorting if needed (it costs time on large datasets)
 */
object Diff {
  def apply[U,K](lefts: Iterable[U], rights: Iterable[U], differ: Differentiator[U,K])(implicit o: Ordering[K]): Seq[(U, U)] = apply(lefts.iterator, rights.iterator, differ)

  def apply[U,K](lefts: Iterator[U], rights: Iterator[U], differ: Differentiator[U,K])(implicit o: Ordering[K]): Seq[(U, U)] = {

    @tailrec
    def eliminateIdenticalElements(left: BeijingIterator[U], right: BeijingIterator[U], key: U => K)(diffs: Seq[(U, U)]): Seq[(U, U)] =
      (left.headOption, right.headOption) match {
        case (None, None) => diffs
        case (None, Some(_)) => right.map((elem: U) => differ.zero -> elem).toSeq ++ diffs
        case (Some(_), None) => left.map((elem: U) => elem -> differ.zero).toSeq ++ diffs
        case (Some(sl), Some(sr)) => (key(sl), key(sr)) match {
          case (l, r) if o.gt(l, r) => eliminateIdenticalElements(left, right.tail, key)((differ.zero, right.head) +: diffs)
          case (l, r) if o.lt(l, r) => eliminateIdenticalElements(left.tail, right, key)((left.head, differ.zero) +: diffs)
          case (l, r) => eliminateIdenticalElements(left.tail, right.tail, key)(if (left.head == right.head) diffs else (left.head, right.head) +: diffs)
        }
      }

    eliminateIdenticalElements(differ.sort(lefts), differ.sort(rights), differ.key)(Nil)
  }
}