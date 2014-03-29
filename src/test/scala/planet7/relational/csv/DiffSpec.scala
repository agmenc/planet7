package planet7.relational.csv

import org.scalatest.WordSpec
import planet7.Diff
import planet7.relational.{FieldSupport, RowSupport, CsvSupport}
import FieldSupport._
import planet7.relational.{RowSupport, CsvSupport}
import RowSupport._
import CsvSupport._

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

    val result: List[(Row, Row)] = Diff(Csv(left).rows, Csv(right).rows, RowDiffer("ID"))

    assert(result === List(
      (Row(List(("ID", "G"), ("Name", "H"), ("Value", "I"))), Row(List(("ID", "G"), ("Name", "X"), ("Value", "I")))),
      (Row(List(("ID", "D"), ("Name", "E"), ("Value", "F"))), RowSupport.EmptyRow)
    ))
  }

  "The result of diffing two rows is a list of field diffs" in {
    val left = List(("ID", "G"), ("Name", "H"), ("Value", "I"))
    val right = List(("ID", "G"), ("Name", "X"), ("Value", "I"))

    val result: List[(Field, Field)] = Diff(left, right, FieldDiffer)

    assert(result === List(("Name", "H") ->("Name", "X")))
  }

  "We know which columns were added and removed" in {
    val left = List(("ID", "G"), ("Name", "H"), ("Removed", "I"))
    val right = List(("Added", "Q"), ("ID", "G"), ("Name", "H"))

    val result: List[(Field, Field)] = Diff(left, right, FieldDiffer)

    assert(result === List(("Removed", "I") -> EmptyField, EmptyField -> ("Added", "Q")))
  }

  // Columns: renamed
  // Results summaries: added, missing, diff (set of column diff counts) ==> all now trivial
  // Identify duplicates in both lists
  // Ability to ignore some columns
  // Ability to set tolerances for numerical field comparisons
}