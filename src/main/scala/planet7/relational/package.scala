package planet7

import planet7.relational.csv.{RowLike, Row}

package object relational {
  type Diffs = List[(RowLike, RowLike)]
}
