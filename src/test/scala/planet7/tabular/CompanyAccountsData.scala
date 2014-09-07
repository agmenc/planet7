package planet7.tabular

import java.io.File

import planet7.tabular.TestData._

object TestData {
  val base = "src/test/resources/planet7/relational/csv"
  def asFile(fileName: String) = new File(s"$base/$fileName")
}

object CompanyAccountsData {
  def postcodeLookupTable = Map((Csv(TestData.asFile("postcodes.csv")).rows map toTuple).to[Seq]:_*)
  private def toTuple(row: Row): (String, String) = row.data.to[List] match {
    case beforeValue :: afterValue :: Nil => beforeValue -> afterValue
    case _ => ???
  }
}

object BeforeAndAfterData {
  import CompanyAccountsData._

  def afterCsv = Csv(asFile("after_with_diffs.csv"))
    .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

  def beforeCsv = Csv(asFile("before.csv"))
    .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
    .withMappings(
      "Postcode" -> postcodeLookupTable,
      "Company" -> (_.toUpperCase)
    )

  def afterSortedCsv = Csv(asFile("after_with_diffs_sorted.csv"))
    .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

  def beforeSortedCsv = Csv(asFile("before_sorted.csv"))
    .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
    .withMappings(
      "Postcode" -> postcodeLookupTable,
      "Company" -> (_.toUpperCase)
    )
}