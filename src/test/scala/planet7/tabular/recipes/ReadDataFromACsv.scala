package planet7.tabular.recipes

import java.io.File

import org.scalatest.{MustMatchers, WordSpec}

class ReadDataFromACsv extends WordSpec with MustMatchers {

  "Loading CSV Data into a more useful data structure" in {
    import planet7.tabular._

    val postcodeData = new File("src/test/resources/planet7/tabular/before/postcodes.csv")

    val lookup = Csv(postcodeData).rows.map {
      case Row(Array(oldCode, newCode)) => oldCode -> newCode.toInt
    }.toMap

    lookup("A1H 9A4") mustEqual 31951
  }
}