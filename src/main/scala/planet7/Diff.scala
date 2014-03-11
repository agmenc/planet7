package planet7

import planet7.relational.csv.{RowLike, Missing, Row, Csv}
import planet7.relational.Diffs

case class Diff(left: Csv, right: Csv)

object Diff {
  def apply(left: Csv, right: Csv, key: List[String]): Diffs = {
    // Validate that the key exists in both CSVs
    // Sort asc, using key as an ordering
    // Extract duplicates (or identify keys with dupe values)

    def extract(key: List[String])(row: Row): String = key.map(row.value).mkString

    def eliminateIdenticalRows(left: List[Row], right: List[Row], key: Row => String)(diffs: List[(RowLike, RowLike)]): Diffs = {
      if (left.isEmpty && right.isEmpty) diffs
      else (key(left.head), key(right.head)) match {
        case (l, r) if l > r => eliminateIdenticalRows(left, right.tail, key)((Missing, right.head) :: diffs)
        case (l, r) if l < r => eliminateIdenticalRows(left.tail, right, key)((left.head, Missing) :: diffs)
        case (l, r) => eliminateIdenticalRows(left.tail, right.tail, key)(if (left.head == right.head) diffs else (left.head, right.head) :: diffs)
      }
    }

    eliminateIdenticalRows(left.rows, right.rows, extract(key))(Nil)
  }
}