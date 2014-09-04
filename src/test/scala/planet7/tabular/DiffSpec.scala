package planet7.tabular

import org.scalatest.{MustMatchers, WordSpec}
import planet7.Diff
import TestData._
import CompanyAccountsData._
import planet7.timing._

class DiffSpec extends WordSpec with MustMatchers {

  "The result of diffing two CSV lists is a list of rows that are different" in {
    val left = Csv("""
                 |ID,Name,Value
                 |A,B,C
                 |D,E,F
                 |G,H,I""".stripMargin)

    val right = Csv("""
                  |ID,Name,Value
                  |A,B,C
                  |G,X,I""".stripMargin)

    val result: Seq[(Row, Row)] = Diff(left.rows, right.rows, RowDiffer(left, "ID"))

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

  private def afterCsv = Csv(asFile("after_with_diffs.csv"))
    .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

  private def beforeCsv = Csv(asFile("before.csv"))
    .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
    .withMappings(
      "Postcode" -> postcodeLookupTable,
      "Company" -> (_.toUpperCase)
    )

  "Diff performs" in {
    import planet7.Diff

    val timer = new Timer(3)
    import timer._

    for (i <- 1 to 53) t"diff" { Diff(beforeCsv, afterCsv, RowDiffer(beforeCsv, "Company ID")) }

    println(timer)
    timer.diff.average must be < 25.0
  }

  // Ability to set tolerances for numerical field comparisons
  // Identify duplicates in both lists
}