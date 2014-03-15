package planet7

import planet7.relational.csv._

/**
 * Opinionated Diff:
 *  - We sort left and right inputs by the provided key, and the results follow this sort order
 *  - The result of a diff is a list of differences, each of which is also (potentially) diffable
 */
object Diff {
  def apply(left: Csv, right: Csv, key: List[String]): List[(Row, Row)] = {
    // Sort asc, using key as an ordering

    // Separate concerns? Pre-validations outside of the diff?
    // Validate that the key exists in both CSVs
    // Extract duplicates (or identify keys with dupe values)

    def extract(key: List[String])(row: Row): String = key.map(row.value).mkString

    def eliminateIdenticalRows(left: List[Row], right: List[Row], key: Row => String)(diffs: List[(Row, Row)]): List[(Row, Row)] = {
      if (left.isEmpty && right.isEmpty) diffs
      else (key(left.head), key(right.head)) match {
        case (l, r) if l > r => eliminateIdenticalRows(left, right.tail, key)((Rows.emptyRow, right.head) :: diffs)
        case (l, r) if l < r => eliminateIdenticalRows(left.tail, right, key)((left.head, Rows.emptyRow) :: diffs)
        case (l, r) => eliminateIdenticalRows(left.tail, right.tail, key)(if (left.head == right.head) diffs else (left.head, right.head) :: diffs)
      }
    }

    eliminateIdenticalRows(left.rows, right.rows, extract(key))(Nil)
  }

  def apply[U](leftD: Diffable[U], rightD: Diffable[U], key: Option[U] => String): List[(U, U)] = {

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