package planet7.relational

import org.scalatest.WordSpec
import com.github.tototoshi.csv.CSVReader
import java.io.StringReader


class QuotedCsvSpec extends WordSpec {
  "Allow externally parsing the CSV so we can handle quoted headers and values" in {

    val input = """ID,"Name,Value"
                  |A,"B,C"
                  |D,"E,F"
                  |G,"H,I"""".stripMargin

    val split: List[List[String]] = CSVReader.open(new StringReader(input)).all()
    val csv = Csv.fromSplitData(split)
    assert(csv.headers === List("ID", "Name,Value"))
    assert(csv.data === List(
        List("A", "B,C"),
        List("D", "E,F"),
        List("G", "H,I")
    ))
  }
}
