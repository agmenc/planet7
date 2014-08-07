package planet7.relational

import java.io.{FileInputStream, InputStream}

import scala.io.Source

object TestData {
  def readFile(name: String): String = Source.fromFile(s"src/test/resources/planet7/relational/csv/$name").getLines().mkString("\n")
  def readFileToInputStream(name: String): InputStream = new FileInputStream(s"src/test/resources/planet7/relational/csv/$name")
}

object CompanyAccountsData {
  def postcodeLookupTable = Map((Csv(TestData.readFile("postcodes.csv")).rows map toTuple).to[Seq]:_*)
  private def toTuple(row: Row): (String, String) = row.values match {
    case beforeValue :: afterValue :: Nil => beforeValue._2 -> afterValue._2
    case _ => ???
  }
}