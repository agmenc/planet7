package planet7.tabular.recipes

import java.io.File

import org.scalatest.{MustMatchers, WordSpec}
import planet7._
import planet7.tabular.CompanyAccountsData.postcodeLookupTable
import planet7.tabular._

class DiffTwoDifferentReportFormats extends WordSpec with MustMatchers {

  val beforeChanges = new File("src/test/resources/planet7/tabular/before/before.csv")
  val afterChanges = new File("src/test/resources/planet7/tabular/after/after_with_diffs_sorted.csv")

  "Diff two reports and analyse the results" in {
    val before = Csv(beforeChanges)
      .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
      .withMappings(
        "Postcode" -> postcodeLookupTable,
        "Company" -> (_.toUpperCase)
      )

    val after = Csv(afterChanges)
      .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

    val diffs: Seq[(Row, Row)] = Diff(before, after, RowDiffer(before.header, "Company ID"))

    val (summary, readableDiffs) = analyse(diffs, before.header, after.header)

    printSummary(summary, readableDiffs)
    assert(readableDiffs === List(
      "Postcode: 43205 -> 432666, Company: ENIM SIT AMET INCORPORATED -> ENIM SIT AMET LIMITED",
      "Postcode: 22656 -> 22756"
    ))
  }

  private def analyse(diffs: Seq[(Row,Row)], leftHeader: Row, rightHeader: Row) = {
    val summary = diffs.groupBy {
      case (row, EmptyRow) => "Missing"
      case (EmptyRow, row) => "Added"
      case (row1, row2) => "Diffs"
    }

    // Diff each row which has changed, zipping in the header information, so we know the field names
    val fieldDifferences = summary("Diffs") map {
      case (leftRow, rightRow) => NonSortingDiff(leftHeader.data zip leftRow.data, rightHeader.data zip rightRow.data, FieldDiffer)
    }

    // Print the name of the field which changed, and the before and after values
    val readableDiffs = fieldDifferences map (FieldDiffer.prettyPrint(_).mkString(", "))

    (summary, readableDiffs)
  }

  private def printSummary(summary: Map[String, Seq[(Row, Row)]], readableDiffs: Seq[String]) = {
    println(s"""\nMissing:${summary("Missing").map(_._1).mkString("\n  -", "\n  -", "")}""")
    println(s"""\nAdded:${summary("Added").map(_._2).mkString("\n  +", "\n  +", "")}""")
    println(s"""\nDiffs:${readableDiffs.mkString("\n  ~", "\n  ~", "")}""")
  }
}