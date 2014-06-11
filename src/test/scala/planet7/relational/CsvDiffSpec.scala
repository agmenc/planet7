package planet7.relational

import org.scalatest.WordSpec
import planet7.Diff
import TestData._
import DefaultRelationalDatasources._

class CsvDiffSpec extends WordSpec {

  "Map long rows with disparate columns to shorter rows containing just the columns to compare" in {
    def toShortRows(fileName: String) = Csv(readFile(fileName)).retainReorderOrAdd("A", "B", "D", "E").rows
    val differ: RowDiffer = RowDiffer("A")

    val result: List[(Row, Row)] = Diff(toShortRows("left.csv"), toShortRows("right.csv"), differ)

    assert(result === List(
      differ.zero -> Row(List("A", "B", "D", "E") zip List("hjt", "waer", "iughv", "7653")),
      Row(List("A", "B", "D", "E") zip List("gfreejuy", "rer", "iu", "642")) -> differ.zero,
      Row(List("A", "B", "D", "E") zip List("", "", "", "")) -> differ.zero
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

    val result: List[(Row, Row)] = Diff(Csv(left).rename("Name" -> "Nickname").rows, Csv(right).rows, RowDiffer("ID"))

    assert(result === List(
      (Row(List(("ID", "D"), ("Nickname", "E"), ("Value", "F"))), Row(List(("ID", "D"), ("Nickname", "Q"), ("Value", "F"))))
    ))
  }

  "Reorder columns between CSVs, so that column position changes don't cause differences" in {
    val left = Csv( """
                      |ID,Name,Value
                      |A,B,C
                      |D,E,F
                      |G,H,I
                    """.stripMargin)

    val right: Csv = Csv( """
                            |ID,Value,Name
                            |A,C,B
                            |D,F,Q
                            |G,I,H
                          """.stripMargin)

    val result: List[(Row, Row)] = Diff(left.retainReorderOrAdd("ID", "Value", "Name").rows, right.rows, RowDiffer("ID"))

    assert(result === List(
      (Row(List(("ID", "D"), ("Value", "F"), ("Name", "E"))), Row(List(("ID", "D"), ("Value", "F"), ("Name", "Q"))))
    ))
  }

  "Map column data to equivalent values (postcode lookup) so that equivalent data rows don't cause differences" in {
    import CompanyAccountsData._

    val before = Csv(readFile("before.csv"))
      .rename("Company account" -> "Company ID")
      .retainReorderOrAdd("First name", "Surname", "Company", "Company ID", "Postcode")
      .remap("Postcode" -> postcodeLookupTable)

    val after = Csv(readFile("after.csv"))
      .retainReorderOrAdd("First name", "Surname", "Company", "Company ID", "Postcode")

    assert(Diff(before.rows, after.rows, RowDiffer("Company ID")) === Nil)
  }
}