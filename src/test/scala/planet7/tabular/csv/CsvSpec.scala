package planet7.tabular.csv

import com.github.tototoshi.csv.CSVReader
import org.scalatest.{MustMatchers, WordSpec}
import planet7.NonSortingDiff
import planet7.tabular._
import planet7.tabular.datasources.CsvReaderDataSource

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

  "We can use external readers as DataSources, such as (the incredibly slow) CsvReader" in {
    import CsvReaderDataSource._
    import planet7.tabular.LargeDataSet._

    val csv = Csv(CSVReader.open(TestDataFile(largeDataFile)))

    csv.header must equal(expectedHeader)
    csv.iterator.next() must be (expectedFirstRow)
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
    result.iterator.toList must equal(threeColumns.iterator.toList)
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

    result.iterator.toList must equal(threeColumns.iterator.toList)
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
                  |val1,val2,val3
                  |0,,""".stripMargin

    noException should be thrownBy export(Csv(input))
  }

  "We can merge Csvs on a key column" in {
    val names = Csv( """ID,First Name,Surname
                          |1,Sue,Smith
                          |3,Bob,Smith
                          |4,Fred,Black
                          |5,Jeremiah,Jones""".stripMargin)

    val ageAndAddress = Csv( """ID,Age,Address
                          |1,24,18 Monkey Street
                          |2,36,94 Elephant Street
                          |4,127,6 Otter Passage
                          |5,36,47 Baboon Way""".stripMargin)

    val expected = Csv( """ID,First Name,Surname,Age,Address
                          |1,Sue,Smith,24,18 Monkey Street
                          |2,[MISSING \"First Name\"],[MISSING \"Surname\"],36,94 Elephant Street
                          |3,Bob,Smith,,
                          |4,Fred,Black,127,6 Otter Passage
                          |5,Jeremiah,Jones,36,47 Baboon Way""".stripMargin)

    merge(on("ID"), names, ageAndAddress)

    fail("nope")
  }

  "Csvs to merge must contain the key in the header" in {
    fail("Nope")
  }
}