package planet7.tabular

import org.scalatest.{MustMatchers, WordSpec}

class ParserSpec extends  WordSpec with MustMatchers {
  "We can change the delimiter when reading a CSV" in {
    val input = "Index\tName\tValue\nD\tE\tF\nG\tH\tI"

    val result = """Index,Name,Value
                   |D,E,F
                   |G,H,I""".stripMargin

    val csv = Csv(fromString(input, Parser('\t')))

    export(csv) mustEqual result
  }

  "We can change the delimiter when writing a CSV" in {
    val input = """Index,Name,Value
                  |D,E,F
                  |G,H,I""".stripMargin

    val result = """Index-Name-Value
                   |D-E-F
                   |G-H-I""".stripMargin

    val csv = Csv(fromString(input))

    export(csv, Parser('-')) mustEqual result
  }
}
