package planet7.tabular

import java.io.{FileInputStream, ByteArrayInputStream}
import java.nio.charset.StandardCharsets

import org.scalatest.{MustMatchers, WordSpec}

import scala.io.Source

class CsvConsistencySpec extends WordSpec with MustMatchers {
  import CsvConsistencySpec._

  "All methods of accessing data produce the same Csv structure" in {
    import planet7.tabular.LargeDataSet._

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
}

object CsvConsistencySpec {
  def possibleLoadMethods(filename: String) = {
    def file = TestData(s"before/$filename")
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
}