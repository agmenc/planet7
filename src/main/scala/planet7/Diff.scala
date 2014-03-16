package planet7

import planet7.relational.csv._
import scala.annotation.tailrec
import planet7.relational.Diffable

/**
 * Opinionated Diff:
 *  - We sort left and right inputs by the provided key, and the results follow this sort order
 *  - The result of a diff is a list of differences, each of which is also (potentially) diffable
 */
object Diff {
  def apply[U](leftD: Diffable[U], rightD: Diffable[U], key: Option[U] => String): List[(U, U)] = {

    @tailrec
    def eliminateIdenticalElements(left: List[U], right: List[U], key: Option[U] => String)(diffs: List[(U, U)]): List[(U, U)] = {
      if (left.isEmpty && right.isEmpty) diffs
      else if (left.isEmpty)  right.map((leftD.zero, _)) ::: diffs
      else if (right.isEmpty) left.map((_, rightD.zero)) ::: diffs
      else (key(left.headOption), key(right.headOption)) match {
        case (l, r) if l > r => eliminateIdenticalElements(left, right.tail, key)((leftD.zero, right.head) :: diffs)
        case (l, r) if l < r => eliminateIdenticalElements(left.tail, right, key)((left.head, rightD.zero) :: diffs)
        case (l, r) => eliminateIdenticalElements(left.tail, right.tail, key)(if (left.head == right.head) diffs else (left.head, right.head) :: diffs)
      }
    }

    eliminateIdenticalElements(leftD.sorted(key), rightD.sorted(key), key)(Nil)
  }
}