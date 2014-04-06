package planet7.relational

import org.scalatest.WordSpec
import planet7.Diff
import FieldSupport._
import RowSupport._
import CsvSupport._
import scala.io.Source

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

  "Map long rows with disparate columns to shorter rows containing just the columns to compare" in {
    def toShortRows(fileName: String) = Csv(readFile(fileName)).keepColumns("A", "B", "D", "E").rows
    def readFile(name: String) = Source.fromFile(s"src/test/resources/planet7/relational/csv/$name").getLines().mkString("\n")
    val differ: RowDiffer = RowDiffer("A")

    val result: List[(Row, Row)] = Diff(toShortRows("left.csv"), toShortRows("right.csv"), differ)

    assert(result === List(
      differ.zero -> Row(List("A", "B", "D", "E") zip List("hjt", "waer", "iughv", "7653")),
      Row(List("A", "B", "D", "E") zip List("gfreejuy", "rer", "iu", "642")) -> differ.zero,
      differ.zero -> differ.zero
    ))
  }

  "Rename columns between CSVs, so that simple column name changes don't cause differences" in {
    val left = """
                 |ID,Name,Value
                 |A,B,C
                 |D,E,F
                 |G,H,I
               """.stripMargin

    val right = """
                  |ID,Nickname,Value
                  |A,B,C
                  |D,Q,F
                  |G,H,I
                """.stripMargin

    val result: List[(Row, Row)] = Diff(Csv(left).renameColumns("Name" -> "Nickname").rows, Csv(right).rows, RowDiffer("ID"))

    assert(result === List(
      (Row(List(("ID", "D"), ("Nickname", "E"), ("Value", "F"))), Row(List(("ID", "D"), ("Nickname", "Q"), ("Value", "F"))))
    ))
  }

  "Reorder columns between CSVs, so that column position changes don't cause differences" in {
    val left = Csv("""
                 |ID,Name,Value
                 |A,B,C
                 |D,E,F
                 |G,H,I
               """.stripMargin).renameColumns("Name" -> "Nickname")

    val right: Csv = Csv("""
                  |ID,Value,Nickname
                  |A,C,B
                  |D,F,Q
                  |G,I,H
                """.stripMargin)

    val result: List[(Row, Row)] = Diff(left.keepColumns("ID", "Value", "Nickname").rows, right.rows, RowDiffer("ID"))

    assert(result === List(
      (Row(List(("ID", "D"), ("Value", "F"), ("Nickname", "E"))), Row(List(("ID", "D"), ("Value", "F"), ("Nickname", "Q"))))
    ))
  }

  "Map column data to equivalent values (postcode lookup) so that equivalent data rows don't cause differences" in {
    object CompanyAccountsData {
      def readFile(name: String) = Source.fromFile(s"src/test/resources/planet7/relational/csv/$name").getLines().mkString("\n")
      def postcodeLookupTable = Map(Csv(readFile("postcodes.csv")).rows map toTuple:_*)
      private def toTuple(row: Row): (String, String) = row.values match {
        case beforeValue :: afterValue :: Nil => beforeValue._2 -> afterValue._2
      }
    }

    import CompanyAccountsData._

    val before = Csv(readFile("before.csv"))
      .renameColumns("Company account" -> "Company ID")
      .keepColumns("First name", "Surname", "Company", "Company ID", "Postcode")
      .withMappings("Postcode" -> postcodeLookupTable)

    val after = Csv(readFile("after.csv"))
      .keepColumns("First name", "Surname", "Company", "Company ID", "Postcode")

    assert(Diff(before.rows, after.rows, RowDiffer("Company ID")) === Nil)
  }

  "Processing a list of Row Diffs to get a list of fields diffs" in {
    val rowDiffs = List(
      (Row(List(("ID", "G"), ("Name", "H"), ("Value", "I"))), Row(List(("ID", "G"), ("Name", "X"), ("Value", "I")))),
      (Row(List(("ID", "D"), ("Name", "E"), ("Value", "F"))), RowSupport.EmptyRow)
    )

    val moreFieldDiffs = rowDiffs map {
      case (row, RowSupport.EmptyRow) => row.values map (_ -> EmptyField)
      case (RowSupport.EmptyRow, row) => row.values map (EmptyField -> _)
      case (left, right) => Diff(left.values, right.values, FieldDiffer)
    }

    assert(moreFieldDiffs === List(
      List(("Name", "H") -> ("Name", "X")),
      List(("ID", "D") -> EmptyField, ("Name", "E") -> EmptyField, ("Value", "F") -> EmptyField)
    ))
  }

  // Results summaries: added, missing, diff (set of column diff counts) ==> all now trivial
  // Identify duplicates in both lists
  // Ability to set tolerances for numerical field comparisons
}