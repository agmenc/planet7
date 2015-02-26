package planet7.tabular.diff

import org.scalatest.{MustMatchers, WordSpec}
import planet7.tabular._
import planet7.timing._
import planet7.{NonSortingDiff, Diff}

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

    val result: Seq[(Row, Row)] = Diff(left.iterator, right.iterator, RowDiffer(left.header, "ID"))

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

    val toFieldDiffs = (l: Row, r: Row) => NonSortingDiff(headers zip l.data, headers zip r.data, FieldDiffer)

    assert((rowDiffs map toFieldDiffs.tupled) === List(
      List(("Name", "H") -> ("Name", "X")),
      List(("ID", "D") -> FieldDiffer.zero, ("Name", "E") -> FieldDiffer.zero, ("Value", "F") -> FieldDiffer.zero)
    ))
  }

  "Diff performs" in {
    import planet7.Diff
    import planet7.tabular.BeforeAndAfterData._

    val timer = new Timer(3)
    import timer._

    for (i <- 1 to 53) t"diff" { Diff(beforeCsv, afterCsv, RowDiffer(beforeCsv.header, "Company ID")) }

    println(timer)
    timer.diff.average must be < 25.0
  }

  "Pre-sorted data does not have to be sorted by Diff" in {
    import planet7.tabular.BeforeAndAfterData._

    val unsortedDiffs = Diff(beforeCsv, afterCsv, RowDiffer(beforeCsv.header, "Company ID"))
    val presortedDiffs = Diff(beforeSortedCsv, afterSortedCsv, RowDiffer(beforeCsv.header, "Company ID"))

    presortedDiffs must equal(unsortedDiffs)
  }

  "Using non-sorting or sorting Differentiators provides the same result for a sorted dataset" in {
    import planet7.tabular.LargeDataSet._

    val timer = new Timer(3)
    import timer._

    val differ = RowDiffer(largeCsv.header, "id" -> by(_.toInt))

    for (i <- 1 to 13) {
      val sortingDiffer = t"sortingDiffer" { Diff(largeCsv, largeCsvWithDiff, differ) }
      val nonSortingDiffer = t"nonSortingDiffer" { NonSortingDiff(largeCsv, largeCsvWithDiff, differ) }

      nonSortingDiffer must equal(sortingDiffer)
    }

    println(timer)

    timer.nonSortingDiffer.average must be < timer.sortingDiffer.average
  }

  "We can Diff Csv instances and generate readable output" in {
    import planet7.Diff
    import planet7.tabular.CompanyAccountsData._

    val before = Csv(Before.asFile("before.csv"))
      .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
      .withMappings(
        "Postcode" -> postcodeLookupTable,
        "Company" -> (_.toUpperCase)
      )

    val after = Csv(After.asFile("after_with_diffs_sorted.csv"))
      .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

    val diffs: Seq[(Row, Row)] = Diff(before, after, RowDiffer(before.header, "Company ID"))

    // The resulting diffs are yours to play with. Let's group them: missing rows, added rows, or just plain different rows.
    val summary = diffs.groupBy {
      case (row, EmptyRow) => "Missing"
      case (EmptyRow, row) => "Added"
      case (row1, row2) => "Diffs"
    }

    // We can Diff rows which have changed. We zip the header information with each row, so that we know the names of the fields which changed.
    val fieldDifferences = summary("Diffs") map {
      case (leftRow, rightRow) => NonSortingDiff(before.header.data zip leftRow.data, after.header.data zip rightRow.data, FieldDiffer)
    }

    // Let's print the name of the field which changed, and the before and after values
    val readableDiffs = fieldDifferences map (FieldDiffer.prettyPrint(_).mkString(", "))
    printSummary(summary, readableDiffs)
    assert(readableDiffs === List(
      "Postcode: 43205 -> 432666, Company: ENIM SIT AMET INCORPORATED -> ENIM SIT AMET LIMITED",
      "Postcode: 22656 -> 22756"
    ))
  }

  private def printSummary(summary: Map[String, Seq[(Row, Row)]], readableDiffs: Seq[String]) = {
    println(s"""\nMissing:${summary("Missing").map(_._1).mkString("\n  -", "\n  -", "")}""")
    println(s"""\nAdded:${summary("Added").map(_._2).mkString("\n  +", "\n  +", "")}""")
    println(s"""\nDiffs:${readableDiffs.mkString("\n  ~", "\n  ~", "")}""")
  }

  "We can match the positions of columns names in a Seq" in {
    val left = Seq("a","c","d","e","f","x","y","h","i", "j")
    val right = Seq("a","b","c","d","f","g","h","i")

    val matched: (Seq[String], Seq[String]) = new RowPrinter().matchPositions(left, right, Nil).reverse.unzip

    matched._1 mustEqual Seq("a", "*", "c", "d", "e", "f", "x", "y", "h", "i", "j")
    matched._2 mustEqual Seq("a", "b", "c", "d", "*", "f", "g", "*", "h", "i", "*")
  }

  "We can show different rows under-and-over, so that we can see the diffs" in {
    val left = Row(Array("some", "", "data", "12345", "here"))
    val right = Row(Array("some", "other", "123", "data", "123", "here"))

    showDiffs(left, right) mustEqual
      """
        |some,[]     ,     ,data,[12345],here
        |some,[other],[123],data,[123]  ,here
        |""".stripMargin
  }
}