package planet7.relational

import scala.io.Source
import DefaultRelationalDatasources._

object TestData {
  def readFile(name: String) = Source.fromFile(s"src/test/resources/planet7/relational/csv/$name").getLines().mkString("\n")
}

object CompanyAccountsData {
  def postcodeLookupTable = Map(Csv(TestData.readFile("postcodes.csv")).rows map toTuple:_*)
  private def toTuple(row: Row): (String, String) = row.values match {
    case beforeValue :: afterValue :: Nil => beforeValue._2 -> afterValue._2
    case _ => ???
  }
}