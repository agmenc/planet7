package planet7.tabular

import org.scalatest.{WordSpec, MustMatchers}

class CsvParsingSpec extends WordSpec with MustMatchers {
  "Adjacent commas create empty data fields" in {
    toRow("0,,,,") must equal (Row(Array("0", "", "", "", "")))
  }
}