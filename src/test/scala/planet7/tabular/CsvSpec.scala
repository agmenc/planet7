package planet7.tabular

import com.github.tototoshi.csv.CSVReader
import org.scalatest.{MustMatchers, WordSpec}
import planet7.NonSortingDiff

class CsvSpec extends WordSpec with MustMatchers {
  "We can construct a Csv from a RelationalInputSource, including blank rows" in {
    val data = """
                 |
                 |
                 |
                 |Some,Header,Columns
                 |
                 |
                 |D,E,F
                 |
                 |G,H,I
                 |
               """.stripMargin.trim

    val unblankedData = """Some,Header,Columns
                          |D,E,F
                          |G,H,I""".stripMargin

    val csv = Csv(data)
    csv.header.toString mustEqual "Some,Header,Columns"
    export(csv) mustEqual unblankedData
  }

  "We can rename and restructure the columns in a Csv" in {
    val data = """Index,Name,Value
                 |D,E,F
                 |G,H,I""".stripMargin

    val result = """Amount,Name
                 |F,E
                 |I,H""".stripMargin

    val csv = Csv(data).columnStructure(
      "Value" -> "Amount", "Name"
    )

    export(csv) mustEqual result
  }

  "We can ignore columns in a Csv" in {
    val data = """Index,Name,Value
                 |D,E,F
                 |G,H,I""".stripMargin

    val result = """Value
                 |F
                 |I""".stripMargin

    val csv = Csv(data).columnStructure(ignore("Name", "Index"))

    export(csv) mustEqual result
  }

  "We can use external parsers such as (the incredibly slow) CsvReader" in {
    import planet7.tabular.LargeDataSet._
    import planet7.tabular.DataSourceAdapters._

    val csv = Csv(CSVReader.open(TestDataFile(largeDataFile)))

    csv.header must equal(expectedHeader)
    csv.rows.next() must be (expectedFirstRow)
  }

  "We can add empty columns to a Csv" in {
    val twoColumns = Csv("""
                       |Name,ID
                       |B,A
                     """.stripMargin)

    val threeColumns = Csv("""
                         |ID,Value,Name
                         |A,,B
                       """.stripMargin)

    val result = twoColumns.columnStructure("ID", "Value", "Name")

    result.header mustEqual Row(Array("ID", "Value", "Name"))
    result.rows.toList must equal(threeColumns.rows.toList)
  }

  "We can map data and specify default values by column" in {
    val twoColumns = Csv("""
                           |Name,ID
                           |BOB,A
                           |DAVE,C
                         """.stripMargin)

    val threeColumns = Csv("""
                             |ID,Value,Name
                             |1,X,bob
                             |3,X,dave
                           """.stripMargin)

    def alphaToNum(alpha: String): String = alpha match {
      case "A" => "1"
      case _ => "3"
    }

    val result = twoColumns
      .columnStructure("ID", "Value", "Name")
      .withMappings(
        "Value" -> (_ => "X"),      // Default value (for the empty column we added)
        "ID" -> alphaToNum,         // Mapping function
        "Name" -> (_.toLowerCase)   // Mapping function
      )

    result.rows.toList must equal(threeColumns.rows.toList)
  }

  "Exporting a Csv generates output which could be read by any other CSV parser" in {
    val rows = List("foo,bar", "one,two", "uno,dos", "ichi,ni", "eins,zwei")

    val expected = """foo,bar
                   |one,two
                   |uno,dos
                   |ichi,ni
                   |eins,zwei""".stripMargin

    export(Csv(rows)) must equal (expected)
  }

  "We can filter rows according to a column-based predicate" in {
    val input = """ID,Name,Value
                  |A,B,C
                  |D,E,F
                  |G,H,I
                  |J,K,L
                  |M,N,O""".stripMargin

    val expectedOutput = """ID,Name,Value
                           |A,B,C
                           |G,H,I
                           |M,N,O""".stripMargin

    val transformedCsv = Csv(input).filter(
      "ID" -> (_ != "D"),
      "Value" -> (List("C", "I", "O") contains _)
    )

    export(transformedCsv) must equal (expectedOutput)
  }

  "We can merge CSVs, so that we can gather similar data from multiple sources" in {
    val left = Csv( """
                      |ID,Name,Value
                      |A,B,C
                    """.stripMargin)

    val middle = Csv( """
                        |ID,Value,Name
                        |D,F,E
                      """.stripMargin).columnStructure("ID", "Name", "Value") // Put columns into the same order

    val right = Csv( """
                       |ID,Name,Value
                       |G,H,I
                     """.stripMargin)

    assert(Csv(left, middle, right).toString === Csv( """
                                                        |ID,Name,Value
                                                        |A,B,C
                                                        |D,E,F
                                                        |G,H,I""".stripMargin).toString)
  }

  "Extract a CSV, remodel it, and convert the data" in {
    import planet7.tabular.CompanyAccountsData._

    // CSV file with header: First name,Surname,Company,Company account,Postcode,Pet names
    val someFile = TestDataFile("before.csv")

    // Retain only three of the original columns, in a different order, renaming
    // "Postcode" to "Zip code", and adding "Fee owing"
    val reshapedCsv = Csv(someFile)
      .columnStructure("Surname", "First name", "Postcode" -> "Zip code", "Fee owing")
      .withMappings(
        "Zip code" -> postcodeLookupTable,  // Map the old postcodes to zip codes, using a Map
        "Surname" -> (_.toUpperCase),       // Make all surnames upper case
        "Fee owing" -> (_ => "0.00")        // Add a default value for "Fee owing" of 0.00
      )

    // Now convert the data to your data model, or export to a feed, or reconcile against another source, etc.
    // reshapedCsv.rows map ( ... )
  }

  "We can Diff Csvs directly" in {
    import planet7.Diff

    val left = Csv("""Some,Header,Columns
                 |D,E,F
                 |G,H,I""".stripMargin)

    val right = Csv("""Some,Header,Columns
                 |D,E,F
                 |G,x,I""".stripMargin)

    val results = Diff(left, right, RowDiffer(left.header, "Some"))

    results must contain (Row(Array("G", "H", "I")) -> Row(Array("G", "x", "I")))
  }

  "We can sort a Csv by column names, converting non-Strings to appropriate types" in {
    val input = Csv( """First Name,Surname,Age
                     |Sue,Smith,24
                     |Jim,Jones,36
                     |Bob,Smith,24
                     |Fred,Black,127
                     |Jeremiah,Jones,36""".stripMargin)

    val expectedOutput = Csv( """First Name,Surname,Age
                                |Bob,Smith,24
                                |Sue,Smith,24
                                |Jeremiah,Jones,36
                                |Jim,Jones,36
                                |Fred,Black,127""".stripMargin)

    val result = sort(input,
      "Age" -> by(_.toInt),
      "Surname",
      "First Name" -> by(_.substring(0, 3))
    )

    export(result) must equal (export(expectedOutput))
  }

  "We can sort a Csv with a non-alpha sort" in {
    import planet7.tabular.LargeDataSet._

    val randomisedCsv = largeCsvUnsorted
    val preSortedCsv = largeCsv

    val explicitlySortedCsv = sort(randomisedCsv, "id" -> by(_.toInt))

    val diffs: Seq[(Row, Row)] = NonSortingDiff(explicitlySortedCsv, preSortedCsv, RowDiffer(preSortedCsv.header, "id" -> by(_.toInt)))
    assert(diffs.size === 0)
    // x must equal (0) // "mustBe empty" gives useless failure messages
  }

  "Empty fields should be marked by commas when exported" in {
    val input = """
                  |val2,val3,val4
                  |0,,
                  | """.stripMargin

    val expected = """
                     |val2,val3,val4
                     |0,,
                     | """.stripMargin.trim

    val twoColumns = Csv(input)

    export(twoColumns) must equal (expected)
  }

  "Handles empty cells gracefully" in {
    val input = """
                  |val2,val3,val4
                  |0,,""".stripMargin

    val csv = Csv(input).columnStructure("val2", "val3", "val4")

    noException should be thrownBy export(csv)
  }
}