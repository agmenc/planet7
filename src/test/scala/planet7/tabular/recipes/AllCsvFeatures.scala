package planet7.tabular.recipes

import java.io.{FileWriter, File}

import org.scalatest.{MustMatchers, WordSpec}
import planet7.Diff

class AllCsvFeatures extends WordSpec with MustMatchers {
  import planet7.tabular._

  val postcodesPath = "src/test/resources/planet7/tabular/before/postcodes.csv"
  val inputPath = "src/test/resources/planet7/tabular/before/old_company_format.csv"
  val outputPath = "target/remastered.csv"
  val modelAnswerPath = "src/test/resources/planet7/tabular/after/remastered_company_format.csv"

  val postcodeLookupTable: Map[String, String] = Csv(new File(postcodesPath)).iterator.map {
    case Row(Array(oldCode, newCode), _) => oldCode -> newCode
  }.toMap

  def validZipCode(zip: String): Boolean = zip.length == 5

  // divert(0)dnl
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
      .assertAndReport(                     // Reported validations are appended to the row
        "Zip code" -> validZipCode _)       // Report any invalid zip codes
      .columnStructure(ignore("Zip code"))  // Drop column, now we've validated against it

    write(sort(csv), outputPath)            // Sort the output and write to disk

    Diff(Csv(new File(outputPath)), Csv(new File(modelAnswerPath)), NaiveRowDiffer) mustBe empty
  }
  // divert(-1)dnl
}