package planet7

import scala.annotation.tailrec
import planet7.relational.Differentiator

/**
 * Opinionated Diff:
 *  - We sort left and right inputs by a key, and so the results follow the same order
 *  - The result of a diff is a Seq of differences, each of which is also (potentially) diffable
 */
object Diff {
  def apply[U](lefts: Seq[U], rights: Seq[U], differ: Differentiator[U]): Seq[(U, U)] = {

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

    eliminateIdenticalElements(lefts.sortBy(differ.key), rights.sortBy(differ.key), differ.keyOrEmpty)(Nil)
  }
}