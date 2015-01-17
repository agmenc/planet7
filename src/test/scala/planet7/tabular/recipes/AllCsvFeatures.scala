package planet7.tabular.recipes

import java.io.{FileWriter, File}

import org.scalatest.{MustMatchers, WordSpec}

class AllCsvFeatures extends WordSpec with MustMatchers {
  import planet7.tabular._

  val postcodeLookupTable = Csv(new File("src/test/resources/planet7/tabular/before/postcodes.csv")).iterator.map {
    case Row(Array(oldCode, newCode), _) => oldCode -> newCode
  }.toMap

  val inputPath = "src/test/resources/planet7/tabular/before/old_company_format.csv"
  val outputPath = "target/remastered.csv"

  def validZipCode(zip: String): Boolean = zip.length < 5

  "All available CSV-manipulation features" in {
    import planet7.tabular._

    val csv = Csv(new File(inputPath))
      .assertAndAbort(                      // Fail-fast validations
        Validations.rowNotTruncated)        // Let's check early, before we select columns
      .columnStructure(
        "Company",                          // Keep any column we name; remove the rest
        "Company account" -> "Company ID",  // Rename "Company account" to "Company ID"
        "First name",
        "Surname",
        "Postcode" -> "Zip code")
      .withMappings(
        "Zip code" -> postcodeLookupTable,  // Specify a (String) => String to change data
        "Surname" -> (_.toUpperCase))
      .assertAndReport(                     // Reporting-only validations
        "Zip code" -> validZipCode _)       // Report any invalid zip codes
      .columnStructure(ignore("Zip code"))  // Drop column, now we've validated against it

    write(sort(csv), outputPath)

    // Diff(Csv(outputPath), Csv(modelAnswers), RowDiffer())
  }

  // TODO - CAS - 13/01/15 - DataSink
  def write(csv: Csv, path: String) = {
    val writer = new FileWriter(path)
    def writeRow(row: Row) = writer.write(s"${row.toString}\n".toCharArray)
    writeRow(csv.header)
    csv.iterator.foreach(writeRow)
    writer.flush()
    writer.close()
  }
}