package planet7.relational

import org.scalatest.WordSpec
import com.github.tototoshi.csv.CSVReader
import java.io.StringReader

class AugmentedCsvParsingSpec extends WordSpec {
  "Allow us to pimp external CSV parsers so we can use them to handle quoted headers and values" in {

    implicit class ExternalParserPimp(csvReader: CSVReader) extends RelationalDataSource {
      private val headersAndData = csvReader.all()
      override def headers = headersAndData.head
      override def data = headersAndData.tail
    }

    val input = """ID,"Name,Value"
                  |A,"B,C"
                  |D,"E,F"
                  |G,"H,I"""".stripMargin

    val externalParser = CSVReader.open(new StringReader(input))

    val csv = Csv(externalParser)
    assert(csv.headers === List("ID", "Name,Value"))
    assert(csv.data === List(
        List("A", "B,C"),
        List("D", "E,F"),
        List("G", "H,I")
    ))
  }
}
