package planet7.relational

import org.scalatest.WordSpec

class TransformCsvStructureSpec extends WordSpec {
  val input = """ID,Name,Value
                |A,B,C
                |D,E,F
                |G,H,I""".stripMargin

  val expectedOutput = """foo,value,id,bar
                         |,C,A,
                         |,F,D,
                         |,I,G,
                         |""".stripMargin
  
  "We can perform all whole-column transformations in one go (rename, reorder, drop unwanted columns, add missing columns)" in {
    val transformedCsv = Csv(input)
      .renameAndRestructure("" -> "foo", "Value" -> "value", "ID" -> "id", "" -> "bar")

    assert(transformedCsv.toString === expectedOutput)
  }

  "We can use map to apply transformations to the CSV structure" in {
    val transformedCsv = Csv(input)
      .map(RowTransforms.rename("Value" -> "value", "ID" -> "id"))
      .map(RowTransforms.restructure("foo", "value", "id", "bar"))

    assert(transformedCsv.toString === expectedOutput)
  }
}