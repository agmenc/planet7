package planet7.relational

import org.scalatest.WordSpec
import DefaultRelationalDatasources._

class CsvSpec extends WordSpec {
  "We can construct a list of Rows from a CSV String" in {
    val data = """
                 |Some,Header,Columns
                 |D,E,F
                 |G,H,I
               """.stripMargin

    assert(Csv(data).rows.head === Row(List(("Some", "D"), ("Header", "E"), ("Columns", "F"))))
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

    assert(Csv(data).rows === List(
      Row(List(("Some", "D"), ("Header", "E"), ("Columns", "F"))),
      Row(List(("Some", "G"), ("Header", "H"), ("Columns", "I")))
    ))
  }

  "An empty CSV behaves itself" in {
    assert(Csv("").rows=== Nil)
    assert(Csv("Some,Header,Columns").rows === Nil)
  }

  "Merge CSVs, so that we can gather similar data from multiple sources" in {
    val left = Csv( """
                      |ID,Name,Value
                      |A,B,C
                    """.stripMargin)

    val right = Csv( """
                       |ID,Value,Name
                       |D,F,E
                     """.stripMargin).retainReorderOrAdd("ID", "Name", "Value") // Put columns into the same order

    assert(Csv(left, right) === Csv( """
                                       |ID,Name,Value
                                       |A,B,C
                                       |D,E,F
                                     """.stripMargin))
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
        .retainReorderOrAdd("ID", "Value", "Name")
        .remap("Value" -> (_ => "Some default value"))

    assert(transform(twoColumnsOfData) === Csv(threeColumnsOfData))

  }

  "The default String representation of Csv is the entire contents, formatted as a CSV" in {
    assert(Csv(
      List("foo", "bar"),
      List(
        List("one", "two"),
        List("uno", "dos"),
        List("ichi", "ni"),
        List("eins", "zwei"))).toString ===
      """foo,bar
        |one,two
        |uno,dos
        |ichi,ni
        |eins,zwei
        |""".stripMargin)
  }

  "For debugging and REPL development, Csv provides a truncated String" in {
    assert(Csv(
      List("foo", "bar"),
      List(
        List("one", "two"),
        List("uno", "dos"),
        List("ichi", "ni"),
        List("eins", "zwei"))).toTruncString ===
      """foo,bar
        |one,two
        |uno,dos
        |ichi,ni
        |...""".stripMargin)
  }


}