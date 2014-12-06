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
  def postcodeLookupTable = Map((Csv(Before.asFile("postcodes.csv")).rows map toTuple).to[Seq]:_*)
  private def toTuple(row: Row): (String, String) = row.data.to[List] match {
    case beforeValue :: afterValue :: Nil => beforeValue -> afterValue
    case _ => ???
  }
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