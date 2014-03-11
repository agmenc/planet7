package planet7.relational.csv

import org.scalatest.WordSpec
import planet7.Diff
import planet7.relational.Diffs

class CsvDiffSpec extends WordSpec {
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

  private val key = List("ID")

  "The result of diffing two CSV lists is a list of differences" in {
    val result: Diffs = Diff(Csv(left), Csv(right), key)

    assert(result === List(
      (Row(List(("ID", "G"), ("Name", "H"), ("Value", "I"))), Row(List(("ID", "G"), ("Name", "X"), ("Value", "I")))),
      (Row(List(("ID", "D"), ("Name", "E"), ("Value", "F"))), Missing)
    ))
  }

  // Results summaries: added, missing, diff (set of column diff counts) ==> all now trivial

  // Identify duplicates in both lists
  // Keys made of sets of columns, to identify matches
  // Ability to ignore some columns
  // Ability to set tolerances for some comparisons
}