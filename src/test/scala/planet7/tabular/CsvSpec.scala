package planet7.tabular

import java.io._
import java.nio.charset.StandardCharsets
import java.util.Comparator

import com.github.tototoshi.csv.CSVReader
import org.scalatest.{MustMatchers, WordSpec}
import TestData._
import planet7.Diff

import scala.io.Source

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

    csv.header mustEqual Row(Array("Amount", "Name"))
    export(csv) mustEqual result
  }

  def possibleLoadMethods(filename: String) = {
    def file = asFile(filename)
    def string = Source.fromFile(file).mkString

    Map[String, () => TabularDataSource](
      "exp. scanner" -> (() => experimentalFromScanner(file)),
      "exp. wholeFile" -> (() => experimentalFromWholeFile(file)),
      "string" -> (() => fromString(string)),
      "stringInputStream" -> (() => fromInputStream(new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8)))),
      "file" -> (() => fromFile(file)),
      "fileInputStream" -> (() => fromInputStream(new FileInputStream(file))),
      "exp. memoryMappedFile" -> (() => experimentalFromMemoryMappedFile(file))
    )
  }

  "All methods of accessing data produce the same Csv structure" in {
    import LargeDataSet._

    for ((label, loadMethod) <- possibleLoadMethods(largeDataFile)) {
      val csv = Csv(loadMethod())
      csv.header must equal(expectedHeader)

      val allRowsMaterialised = csv.rows.to[List]
      allRowsMaterialised.size must be (expectedRowCount)
      allRowsMaterialised.head must be (expectedFirstRow)
      allRowsMaterialised.last must be (expectedLastRow)
    }
  }

  "All methods of accessing data handle empty files correctly" in {
    for {
      filename <- Seq("completely-empty.csv", "blank-lines.csv")
      (label, loadMethod) <- possibleLoadMethods(filename)
    } a [NoDataInSourceException] should be thrownBy Csv(loadMethod()).header
  }

  "All methods of accessing data handle header-only files correctly" in {
    for {
      filename <- Seq("header-only.csv", "header-and-blank-lines.csv")
      (label, loadMethod) <- possibleLoadMethods(filename)
    } {
      val csv = Csv(loadMethod())

      csv.header must equal(Row(Array("First name", "Surname", "Company", "Company account", "Postcode", "Pet names")))
      csv.rows mustBe empty
    }
  }

  /**
   * Data is sourced from Mockaroo. To regenerate:
   * curl http://www.mockaroo.com/7aa9b980/download?count=1000 > "My Saved Schema.csv"
   *
   * Typical results:

           exp. scanner       273.94 ms (avg. of 17 readings)
      stringInputStream       259.76 ms (avg. of 17 readings)
                 string       230.18 ms (avg. of 17 readings)
         exp. wholeFile       166.00 ms (avg. of 17 readings)
  exp. memoryMappedFile       120.65 ms (avg. of 17 readings)
                   file        96.24 ms (avg. of 17 readings)
        fileInputStream        93.47 ms (avg. of 17 readings)

   */
  "Performance test for different file-access methods" in {
    import LargeDataSet._
    import planet7.timing._

    def processLargeDataset(datasource: TabularDataSource) = export(
      Csv(datasource)
        .columnStructure("first_name" -> "First Name", "last_name", "fee paid")
        .withMappings("last_name" -> (_.toUpperCase))
    )

    val timer = new Timer(3)
    import timer._

    for {
      (label, loadMethod) <- possibleLoadMethods(largeDataFile)
      i <- 1 to 20
    } t"$label" {
      if (i == 1) println(label)
      processLargeDataset(loadMethod())
    }

    println(timer)
    timer.file.average must be < 180.0
  }

  // 143 seconds to load 25000 rows, i.e. 1,000 times slower than just reading the file into Csv Rows. Hells bells.
  "We can use external parsers such as (the incredibly slow) CsvReader" in {
    import LargeDataSet._

    val csv = Csv(CSVReader.open(asFile(largeDataFile)))

    csv.header must equal(expectedHeader)
    csv.rows.next() must be (expectedFirstRow)
  }

  implicit def fromCsvReader(reader: CSVReader): TabularDataSource = new TabularDataSource {
    override val header = reader.readNext() match {
      case Some(items) => Row(items.toArray)
      case None => throw new NoDataInSourceException(reader.toString)
    }

    override def rows = reader.iterator.map(items => Row(items.toArray))

    override def close() = reader.close()
  }

  "Users of the planet7 library can gauge the performance impact of external parsers such as CsvReader" in {
    import planet7.timing._

    val timer = new Timer(2)
    import timer._

    for (i <- 1 to 5) {
      t"overallTime" {
        val csvReader = t"shouldBeQuick" { CSVReader.open(asFile("before.csv")) }
        val csv = t"shouldAlsoBeQuick" { Csv(csvReader) }
        t"veryExpensive" { export(csv) }
      }
    }

    println(timer)
    timer.overallTime.average must be < 150.0
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

  "We can Diff Csv instances and generate readable output" in {
    import planet7.Diff
    import CompanyAccountsData._

    val before = Csv(asFile("before.csv"))
      .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
      .withMappings(
        "Postcode" -> postcodeLookupTable,
        "Company" -> (_.toUpperCase)
      )

    val after = Csv(asFile("after_with_diffs.csv"))
      .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

    val diffs: Seq[(Row, Row)] = Diff(before.rows, after.rows, SortingRowDiffer(before.header, "Company ID"))

    // The resulting diffs are yours to play with. Let's group them: missing rows, added rows, or just plain different rows
    val summary = diffs.groupBy {
      case (row, EmptyRow) => "Missing"
      case (EmptyRow, row) => "Added"
      case (row1, row2) => "Diffs"
    }

    // We can Diff rows which have changed. We zip the header information with each row, so that we know the names of the fields which changed.
    val fieldDifferences = summary("Diffs") map {
      case (leftRow, rightRow) => Diff(before.header.data zip leftRow.data, after.header.data zip rightRow.data, FieldDiffer)
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

  "Extract a CSV, remodel it, and convert the data" in {
    import CompanyAccountsData._

    // CSV file with header: First name,Surname,Company,Company account,Postcode,Pet names
    val someFile = asFile("before.csv")

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

    val results = Diff(left, right, SortingRowDiffer(left.header, "Some"))

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
      "Surname" -> by(identity),
      "First Name" -> by(identity)
    )

    export(result) must equal (export(expectedOutput))
  }

  "We can sort a Csv with a non-alpha sort" in {
    import LargeDataSet._

    val randomisedCsv = largeCsvUnsorted
    val preSortedCsv = largeCsv

    val explicitlySortedCsv = sort(randomisedCsv, "id" -> by(_.toInt))

    val diffs: Seq[(Row, Row)] = Diff(explicitlySortedCsv, preSortedCsv, NonSortingRowDiffer(0))
    assert(diffs.size === 0)
    // x must equal (0) // "mustBe empty" gives useless failure messages
  }

  "Ordered and Ordering" in {
    def sort1[U : Ordering](it: Iterator[U]): Iterator[U] = it.toSeq.sorted.iterator
    def sort2[U : Ordering](it: Iterator[U]): Iterator[U] = it.toSeq.sortBy(identity).iterator
    def sort3[U, K : Ordering](it: Iterator[U], kf: (U) => K): Iterator[U] = it.toSeq.sortBy(kf).iterator

    def ints = List(22, 1, 4, 2).iterator
    println(sort1(ints).mkString(","))
    println(sort2(ints).mkString(","))
    println(sort3(ints, identity[Int]).mkString(","))
  }
}