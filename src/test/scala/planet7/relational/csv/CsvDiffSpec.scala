package planet7.relational.csv

import org.scalatest.WordSpec
import planet7.Diff
import planet7.relational.csv.Fields._

class CsvDiffSpec extends WordSpec {

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

    val result: List[(Row, Row)] = Diff(Csv(left), Csv(right), List("ID"))

    assert(result === List(
      (Row(List(("ID", "G"), ("Name", "H"), ("Value", "I"))), Row(List(("ID", "G"), ("Name", "X"), ("Value", "I")))),
      (Row(List(("ID", "D"), ("Name", "E"), ("Value", "F"))), Rows.emptyRow)
    ))
  }

  "The result of diffing two rows is a list of field diffs" in {
    val left = Row(List(("ID", "G"), ("Name", "H"), ("Value", "I")))
    val right = Row(List(("ID", "G"), ("Name", "X"), ("Value", "I")))

    val result: List[(Field, Field)] = Diff[Field](left, right, Rows.key)

    assert(result === List(("Name", "H") ->("Name", "X")))
  }

  "We know which columns were added and removed" in {
    val left = Row(List(("ID", "G"), ("Name", "H"), ("Removed", "I")))
    val right = Row(List(("Added", "Q"), ("ID", "G"), ("Name", "H")))

    val result: List[(Field, Field)] = Diff[Field](left, right, Rows.key)

    assert(result === List(("Removed", "I") -> EmptyField, EmptyField -> ("Added", "Q")))
  }

  // Columns: extra, missing
  // Columns: renamed
  // Results summaries: added, missing, diff (set of column diff counts) ==> all now trivial
  // Identify duplicates in both lists
  // Keys made of sets of columns, to identify matches
  // Ability to ignore some columns
  // Ability to set tolerances for some comparisons
}