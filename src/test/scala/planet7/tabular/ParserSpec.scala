package planet7.tabular

import org.scalatest.{MustMatchers, WordSpec}

class ParserSpec extends  WordSpec with MustMatchers {
  "We can change the delimiter when reading and writing a CSV" in {
    val data = """Index\tName\tValue
                 |D\tE\tF
                 |G\tH\tI""".stripMargin


    val result = """Index-Name-Value
                   |D-E-F
                   |G-H-I""".stripMargin

    val csv = Csv(fromString(data, Parser('\t')))

    export(csv, Parser('-')) mustEqual result
  }
}