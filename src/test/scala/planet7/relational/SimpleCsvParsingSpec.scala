package planet7.relational

import org.scalatest.{MustMatchers, WordSpec}

class SimpleCsvParsingSpec extends WordSpec with MustMatchers {
  "We can construct a list of Rows from a CSV String" in {
    val data = """
                 |Some,Header,Columns
                 |D,E,F
                 |G,H,I
               """.stripMargin

    assert(Csv(data).rows.next === Row(List(("Some", "D"), ("Header", "E"), ("Columns", "F"))))
  }

  "We handle blank rows" in {
    val data = """
                 |
                 |Some,Header,Columns
                 |
                 |
                 |D,E,F
                 |
                 |G,H,I
                 |
               """.stripMargin

    assert(Csv(data).rows.to[Seq] === Seq(
      Row(List(("Some", "D"), ("Header", "E"), ("Columns", "F"))),
      Row(List(("Some", "G"), ("Header", "H"), ("Columns", "I")))
    ))
  }

  "An empty CSV behaves itself" in {
    assert(Csv("").rows.isEmpty)
    assert(Csv("Some,Header,Columns").rows.isEmpty)
  }

  "Merge CSVs, so that we can gather similar data from multiple sources" in {
    val left = Csv( """
                      |ID,Name,Value
                      |A,B,C
                    """.stripMargin)

    val middle = Csv( """
                       |ID,Value,Name
                       |D,F,E
                     """.stripMargin).restructure("ID", "Name", "Value") // Put columns into the same order

    val right = Csv( """
                       |ID,Name,Value
                       |G,H,I
                     """.stripMargin)

    assert(Csv(left, middle, right).toString === Csv( """
                                                        |ID,Name,Value
                                                        |A,B,C
                                                        |D,E,F
                                                        |G,H,I""".stripMargin).toString)
  }

  "We can add columns to CSVs" in {
    val twoColumnsOfData = """
                             |ID,Name
                             |A,B
                           """.stripMargin

    val threeColumnsOfData = """
                               |ID,Value,Name
                               |A,Some default value,B
                             """.stripMargin

    def transform(data: String) =
      Csv(twoColumnsOfData)
        .restructure("ID", "Value", "Name")
        .remap("Value" -> (_ => "Some default value"))

    assert(transform(twoColumnsOfData).toString === Csv(threeColumnsOfData).toString)

  }

  "The default String representation of Csv is the entire contents, formatted as a CSV" in {
    val csv = Csv(List("foo", "bar"), List(
      List("one", "two"),
      List("uno", "dos"),
      List("ichi", "ni"),
      List("eins", "zwei")).iterator)

    assert(csv.toString === """foo,bar
                              |one,two
                              |uno,dos
                              |ichi,ni
                              |eins,zwei
                              |""".stripMargin)
  }

  "For debugging and REPL development, Csv provides a truncated String" in {
    val csv = Csv(List("foo", "bar"), List(
      List("one", "two"),
      List("uno", "dos"),
      List("ichi", "ni"),
      List("eins", "zwei")).iterator)

    assert(csv.toTruncString === """foo,bar
                                   |one,two
                                   |uno,dos
                                   |ichi,ni""".stripMargin)
  }
}