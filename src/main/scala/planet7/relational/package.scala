package planet7

import planet7.relational.csv.RowLike

package object relational {
  type Diffs = List[(RowLike, RowLike)]
}
