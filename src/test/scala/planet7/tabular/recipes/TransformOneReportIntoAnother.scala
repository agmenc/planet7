package planet7.tabular.recipes

import java.io.File

import org.scalatest.{MustMatchers, WordSpec}

class TransformOneReportIntoAnother extends WordSpec with MustMatchers {
  import planet7.tabular._

  private val postcodeLookupTable = Csv(new File("src/test/resources/planet7/tabular/before/postcodes.csv")).iterator.map {
    case Row(Array(oldCode, newCode), _) => oldCode -> newCode
  }.toMap

  private val gravidaExpectedOutput = "GRAVIDA FOUNDATION,TR729188509373842450961594,0.00,39376"

  "How to transform an input format into the desired output format" in {
    val input = new File("src/test/resources/planet7/tabular/before/before.csv")

    val output = Csv(input)
      .columnStructure(
        "Company",                          // Retain
        "Company account" -> "Company ID",  // Rename
        "Fee owing",                        // Add new column
        "Postcode" -> "Zip code")           // Rename
      .withMappings(
        "Zip code" -> postcodeLookupTable,  // Map postcodes to zip codes
        "Company" -> (_.toUpperCase),       // Make all company names upper case
        "Fee owing" -> (_ => "0.00")        // Add a default value for "Fee owing" of 0.00
      )

    export(output).lines.find(_.contains("GRAVIDA")) mustEqual Some(gravidaExpectedOutput)
  }
}