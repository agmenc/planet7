package planet7.relational

import org.scalatest.WordSpec


class TransformCsvSpec extends WordSpec {
  "We can perform all transformations in one go" in {
    val input = """ID,Name,Value
                  |A,B,C
                  |D,E,F
                  |G,H,I""".stripMargin

    val transformedCsv = Csv(input).defineOutputColumns("" -> "foo", "Value" -> "value", "ID" -> "id", "" -> "bar")

    assert(transformedCsv.toCsvString === """foo,value,id,bar
                                            |,C,A,
                                            |,F,D,
                                            |,I,G,
                                            |""".stripMargin)
  }
}