package planet7.tabular

import java.io._
import java.nio.charset.StandardCharsets

import com.github.tototoshi.csv.CSVReader
import org.scalatest.{MustMatchers, WordSpec}
import planet7.relational.TestData._

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

    def processLargeDataset(datasource: TabularDataSource) = {
      val csv = Csv(datasource)
            .columnStructure("first_name" -> "First Name", "last_name", "fee paid")
      //      .remap("last_name" -> (_.toUpperCase))

      export(csv)
    }

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

//  "An empty Csv2 behaves itself" in {
//    assert(Csv2("").rows=== Nil)
//    assert(Csv2("Some,Header,Columns").rows === Nil)
//  }
//
//  "Merge Csv2s, so that we can gather similar data from multiple sources" in {
//    val left = Csv2( """
//                      |ID,Name,Value
//                      |A,B,C
//                    """.stripMargin)
//
//    val right = Csv2( """
//                       |ID,Value,Name
//                       |D,F,E
//                     """.stripMargin).restructure("ID", "Name", "Value") // Put columns into the same order
//
//    assert(Csv2(left, right) === Csv2( """
//                                       |ID,Name,Value
//                                       |A,B,C
//                                       |D,E,F
//                                     """.stripMargin))
//  }
//
//  "We can add columns to Csv2s" in {
//    val twoColumnsOfData = """
//                             |ID,Name
//                             |A,B
//                           """.stripMargin
//
//    val threeColumnsOfData = """
//                               |ID,Value,Name
//                               |A,Some default value,B
//                             """.stripMargin
//
//    def transform(data: String) =
//      Csv2(twoColumnsOfData)
//        .restructure("ID", "Value", "Name")
//        .remap("Value" -> (_ => "Some default value"))
//
//    assert(transform(twoColumnsOfData) === Csv2(threeColumnsOfData))
//
//  }
//
//  "The default String representation of Csv2 is the entire contents, formatted as a Csv2" in {
//    assert(Csv2(
//      List("foo", "bar"),
//      List(
//        List("one", "two"),
//        List("uno", "dos"),
//        List("ichi", "ni"),
//        List("eins", "zwei"))).toString ===
//      """foo,bar
//        |one,two
//        |uno,dos
//        |ichi,ni
//        |eins,zwei
//        |""".stripMargin)
//  }
//
//  "For debugging and REPL development, Csv2 provides a truncated String" in {
//    assert(Csv2(
//      List("foo", "bar"),
//      List(
//        List("one", "two"),
//        List("uno", "dos"),
//        List("ichi", "ni"),
//        List("eins", "zwei"))).toTruncString ===
//      """foo,bar
//        |one,two
//        |uno,dos
//        |ichi,ni
//        |...""".stripMargin)
//  }
}