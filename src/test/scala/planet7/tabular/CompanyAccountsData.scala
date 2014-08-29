package planet7.tabular

import java.io.File

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