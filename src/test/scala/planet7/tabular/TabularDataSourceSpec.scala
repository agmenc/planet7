package planet7.tabular

import org.scalatest.{MustMatchers, WordSpec}
import planet7.relational.TestData._

class TabularDataSourceSpec extends WordSpec with MustMatchers {
  "We cannot read from the same datasource twice" in {
    def file = asFile("large_dataset.csv")

    val possibleLoadMethods = Map(
      //      "string" -> fromString(string),
      "file" -> fromFile(file) //,
      //      "stringInputStream" -> fromInputStream(new ByteArrayInputStream(string.getBytes(StandardCharsets.UTF_8))),
      //      "fileInputStream" -> fromInputStream(new FileInputStream(file)),
      //      "exp. memoryMappedFile" -> experimentalFromMemoryMappedFile(file),
      //      "exp. scanner" -> experimentalFromScanner(file),
      //      "exp. wholeFile" -> experimentalFromWholeFile(file)
    )

    /**
     * Either make Csv Closeable, or close the InputStream when the Iterator is exhausted
     */

    val poo: TabularDataSource = fromFile(file)
    poo.rows.mkString("\n")

    // Should throw exception:
    poo.rows
    //    the [IndexOutOfBoundsException] thrownBy {
    //      s.charAt(-1)
    //    } should have message "String index out of range: -1"

    fail("Write me")
  }
}