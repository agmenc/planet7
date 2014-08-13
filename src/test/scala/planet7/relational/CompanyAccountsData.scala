package planet7.relational

import java.io.{File, FileInputStream, InputStream}

import scala.io.Source

object TestData {
  val base = "src/test/resources/planet7/relational/csv"
  def asString(fileName: String): String = Source.fromFile(s"$base/$fileName").getLines().mkString("\n")
  def asInputStream(fileName: String): InputStream = new FileInputStream(s"$base/$fileName")
  def asFile(fileName: String) = new File(s"$base/$fileName")
}

object CompanyAccountsData {
  def postcodeLookupTable = Map((Csv(TestData.asString("postcodes.csv")).rows map toTuple).to[Seq]:_*)
  private def toTuple(row: Row): (String, String) = row.values.to[List] match {
    case beforeValue :: afterValue :: Nil => beforeValue._2 -> afterValue._2
    case _ => ???
  }
}