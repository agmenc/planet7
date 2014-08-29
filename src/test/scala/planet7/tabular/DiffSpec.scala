package planet7.tabular

import org.scalatest.WordSpec
import planet7.Diff

class DiffSpec extends WordSpec {

  "The result of diffing two CSV lists is a list of rows that are different" in {
    val left = """
                 |ID,Name,Value
                 |A,B,C
                 |D,E,F
                 |G,H,I
               """.stripMargin

    val right = """
                  |ID,Name,Value
                  |A,B,C
                  |G,X,I
                """.stripMargin

    val result: Seq[(Row, Row)] = Diff(Csv(left).rows, Csv(right).rows, RowDiffer(0))

    assert(result === List(
      (Row(Array("G", "H", "I")), Row(Array("G", "X", "I"))),
      (Row(Array("D", "E", "F")), EmptyRow)
    ))
  }

  "The result of diffing two lists of fields is a list of field diffs" in {
    val left = List(("ID", "G"), ("Name", "H"), ("Value", "I"))
    val right = List(("ID", "G"), ("Name", "X"), ("Value", "I"))

    val result = Diff(left, right, FieldDiffer)

    assert(result === List(("Name", "H") ->("Name", "X")))
  }

  "We know which columns were added and removed" in {
    val left = List(("ID", "G"), ("Name", "H"), ("Removed", "I"))
    val right = List(("Added", "Q"), ("ID", "G"), ("Name", "H"))

    val result: Seq[((String, String), (String, String))] = Diff(left, right, FieldDiffer)

    assert(result === List(("Removed", "I") -> FieldDiffer.zero, FieldDiffer.zero ->("Added", "Q")))
  }

  "Convert a list of Row Diffs to a list of Field diffs, so that we can see which columns have changed" in {
    val rowDiffs = List(
      (Row(Array("G", "H", "I")), Row(Array("G", "X", "I"))),
      (Row(Array("D", "E", "F")), EmptyRow)
    )

    val headers = Array("ID", "Name", "Value")

    val toFieldDiffs = (l: Row, r: Row) => Diff(headers zip l.data, headers zip r.data, FieldDiffer)

    assert((rowDiffs map toFieldDiffs.tupled) === List(
      List(("Name", "H") -> ("Name", "X")),
      List(("ID", "D") -> FieldDiffer.zero, ("Name", "E") -> FieldDiffer.zero, ("Value", "F") -> FieldDiffer.zero)
    ))
  }

  // Ability to set tolerances for numerical field comparisons
  // Identify duplicates in both lists
}