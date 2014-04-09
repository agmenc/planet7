package planet7.relational

import org.scalatest.WordSpec
import planet7.Diff
import planet7.relational.FieldSupport._
import planet7.relational.RowSupport._
import planet7.relational.CsvSupport._
import scala.io.Source
import TestData._

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

  "The result of diffing two lists of fields is a list of field diffs" in {
    val left = List(("ID", "G"), ("Name", "H"), ("Value", "I"))
    val right = List(("ID", "G"), ("Name", "X"), ("Value", "I"))

    val result: List[(Field, Field)] = Diff(left, right, FieldDiffer)

    assert(result === List(("Name", "H") ->("Name", "X")))
  }

  "We know which columns were added and removed" in {
    val left = List(("ID", "G"), ("Name", "H"), ("Removed", "I"))
    val right = List(("Added", "Q"), ("ID", "G"), ("Name", "H"))

    val result: List[(Field, Field)] = Diff(left, right, FieldDiffer)

    assert(result === List(("Removed", "I") -> EmptyField, EmptyField ->("Added", "Q")))
  }

  "Map CSV files to case classes representing the columns to compare" in {
    case class ComparisonFields(a: String, b: String, d: String, e: Integer)

    object Empty extends ComparisonFields("", "", "", 0) {
      override def toString = "Empty"
    }

    object CfDiffer extends Differentiator[ComparisonFields] {
      def zero = Empty
      def key(u: ComparisonFields) = u.a
    }

    def toComparisons(fileName: String): List[ComparisonFields] = Csv(readFile(fileName)).rows map createComparison
    def readFile(name: String) = Source.fromFile(s"src/test/resources/planet7/relational/csv/$name").getLines().mkString("\n")
    def createComparison(row: Row) = ComparisonFields(row.value("A"), row.value("B"), row.value("D"), safeInt(row.value("E")))
    def safeInt(s: String): Integer = if (s.isEmpty) 0 else s.toInt

    val result: List[(ComparisonFields, ComparisonFields)] = Diff(toComparisons("left.csv"), toComparisons("right.csv"), CfDiffer)

    assert(result === List(
      CfDiffer.zero -> ComparisonFields("hjt", "waer", "iughv", 7653),
      ComparisonFields("gfreejuy", "rer", "iu", 642) -> CfDiffer.zero,
      ComparisonFields("", "", "", 0) -> CfDiffer.zero
    ))
  }

  "Convert a list of Row Diffs to a list of Field diffs, so that we can see which columns have changed" in {
    val rowDiffs = List(
      (Row(List(("ID", "G"), ("Name", "H"), ("Value", "I"))), Row(List(("ID", "G"), ("Name", "X"), ("Value", "I")))),
      (Row(List(("ID", "D"), ("Name", "E"), ("Value", "F"))), RowSupport.EmptyRow)
    )

    val toFieldDiffs = (l: Row, r: Row) => Diff(l.values, r.values, FieldDiffer)

    assert((rowDiffs map toFieldDiffs.tupled) === List(
      List(("Name", "H") -> ("Name", "X")),
      List(("ID", "D") -> EmptyField, ("Name", "E") -> EmptyField, ("Value", "F") -> EmptyField)
    ))
  }

  "Analyse the differences, so that we can display them clearly to the user" in {
    import CompanyAccountsData._

    val before = Csv(readFile("before.csv"))
      .renameColumns("Company account" -> "Company ID")
      .keepColumns("First name", "Surname", "Company", "Company ID", "Postcode")
      .withMappings("Postcode" -> postcodeLookupTable)

    val after = Csv(readFile("after_with_diffs.csv"))
      .keepColumns("First name", "Surname", "Company", "Company ID", "Postcode")

    val diffs: List[(Row, Row)] = Diff(before.rows, after.rows, RowDiffer("Company ID"))

    val summary = diffs.groupBy {
      case (row, EmptyRow) => "Missing"
      case (EmptyRow, row) => "Added"
      case (row1, row2) => "Diffs"
    }

    val readableDiffs = summary("Diffs") map (d => Diff(d._1.values, d._2.values, FieldDiffer)) map (prettyPrint(_).mkString(", "))

    printSummary(summary, readableDiffs)

    assert(readableDiffs === List(
      "Postcode: 43205 -> 432666, Company: Enim Sit Amet Incorporated -> Enim Sit Amet Limited",
      "Postcode: 22656 -> 22756"
    ))
  }

  def printSummary(summary: Map[String, List[(Row, Row)]], readableDiffs: List[String]) = {
    println(s"""\nMissing:${summary("Missing").map(_._1).mkString("\n  -", "\n  -", "")}""")
    println(s"""\nAdded:${summary("Added").map(_._2).mkString("\n  +", "\n  +", "")}""")
    println(s"""\nDiffs:${readableDiffs.mkString("\n  ~", "\n  ~", "")}""")
  }

  // TODO - CAS - 08/04/2014 - Make field mapper a function, not just a Map

  // Ability to set tolerances for numerical field comparisons
  // Identify duplicates in both lists
}