package planet7.tabular

import java.io.File
import scala.language.dynamics

object TestData {
  val base = "src/test/resources/planet7/tabular"
  def apply(filename: String): File = new File(s"$base/$filename")
}

object TestDataFile {
  def apply(filename: String) = TestData(s"before/$filename")
}

object Before {
  def asFile(filename: String) = TestData(s"before/$filename")
}

object After {
  def asFile(filename: String) = TestData(s"after/$filename")
}

object CompanyAccountsData {
  val postcodeLookupTable = Csv(new File("src/test/resources/planet7/tabular/before/postcodes.csv")).iterator.map {
    case Row(Array(oldCode, newCode), _) => oldCode -> newCode
  }.toMap
}

object BeforeAndAfterData {
  import CompanyAccountsData._

  def beforeCsv = Csv(Before.asFile("before.csv"))
    .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
    .withMappings(
      "Postcode" -> postcodeLookupTable,
      "Company" -> (_.toUpperCase)
    )

  def beforeSortedCsv = Csv(Before.asFile("before_sorted.csv"))
    .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
    .withMappings(
      "Postcode" -> postcodeLookupTable,
      "Company" -> (_.toUpperCase)
    )

  def afterCsv = Csv(After.asFile("after_with_diffs.csv"))
    .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

  def afterSortedCsv = Csv(After.asFile("after_with_diffs_sorted.csv"))
    .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")
}