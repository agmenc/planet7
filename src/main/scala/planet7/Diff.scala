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
  def apply[U](lefts: Iterable[U], rights: Iterable[U], differ: Differentiator[U]): Seq[(U, U)] = apply(lefts.iterator, rights.iterator, differ)

  def apply[U](lefts: Iterator[U], rights: Iterator[U], differ: Differentiator[U]): Seq[(U, U)] = {

    @tailrec
    def eliminateIdenticalElements(left: Seq[U], right: Seq[U], key: Option[U] => String)(diffs: Seq[(U, U)]): Seq[(U, U)] = {
      if (left.isEmpty && right.isEmpty) diffs
      else if (left.isEmpty)  right.map((differ.zero, _)) ++ diffs
      else if (right.isEmpty) left.map((_, differ.zero)) ++ diffs
      else (key(left.headOption), key(right.headOption)) match {
        case (l, r) if l > r => eliminateIdenticalElements(left, right.tail, key)((differ.zero, right.head) +: diffs)
        case (l, r) if l < r => eliminateIdenticalElements(left.tail, right, key)((left.head, differ.zero) +: diffs)
        case (l, r) => eliminateIdenticalElements(left.tail, right.tail, key)(if (left.head == right.head) diffs else (left.head, right.head) +: diffs)
      }
    }

    eliminateIdenticalElements(lefts.to[Seq].sortBy(differ.key), rights.to[Seq].sortBy(differ.key), differ.keyOrEmpty)(Nil)
  }
}