package planet7.tabular.recipes

import java.io.File

import org.scalatest.{MustMatchers, WordSpec}

class TransformOneReportIntoAnother extends WordSpec with MustMatchers {

  import planet7.tabular._
  import planet7.Diff

  val postcodeLookupTable = Csv(new File("src/test/resources/planet7/tabular/before/postcodes.csv")).rows.map {
    case Row(Array(oldCode, newCode)) => oldCode -> newCode
  }.toMap

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
        "Surname" -> (_.toUpperCase),       // Make all surnames upper case
        "Fee owing" -> (_ => "0.00")        // Add a default value for "Fee owing" of 0.00
      )

    export(output).lines.find(_.contains("Gravida")) mustEqual Some("Monkeys")
    output.rows.find{ case Row(Array(company, _, _, _)) if company == "Gravida Foundation" => true } mustEqual Row(Array())
//    Diff(left.rows, right.rows, RowDiffer(left.header, "ID"))
  }
}