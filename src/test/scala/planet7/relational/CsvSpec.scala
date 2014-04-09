package planet7.relational

import org.scalatest.WordSpec
import RowSupport._
import CsvSupport._

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
                     """.stripMargin).keepColumns("ID", "Name", "Value") // Put columns into the same order

    assert(Csv(left, right) === Csv( """
                                       |ID,Name,Value
                                       |A,B,C
                                       |D,E,F
                                     """.stripMargin))
  }
}