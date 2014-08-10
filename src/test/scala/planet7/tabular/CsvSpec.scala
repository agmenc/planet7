package planet7.tabular

import java.io.FileInputStream

import org.scalatest.{MustMatchers, WordSpec}
import planet7.relational.TestData._

class CsvSpec extends WordSpec with MustMatchers {
  "We can construct a Csv from a RelationalInputSource, including blank rows" in {
    val data = """
                 |
                 |Some,Header,Columns
                 |
                 |
                 |D,E,F
                 |
                 |G,H,I
                 |
               """.stripMargin.trim

    val unblankedData = """Some,Header,Columns
                          |D,E,F
                          |G,H,I""".stripMargin

    val csv = Csv(data)
    csv.header.toString mustEqual "Some,Header,Columns"
    export(csv) mustEqual unblankedData
  }

  "We can rename and restructure the columns in a Csv" in {
    val data = """Index,Name,Value
                 |D,E,F
                 |G,H,I""".stripMargin

    val result = """Amount,Name
                 |F,E
                 |I,H""".stripMargin

    val csv = Csv(data).columnStructure(
      "Value" -> "Amount", "Name"
    )

    csv.header mustEqual Row(Array("Amount", "Name"))
    export(csv) mustEqual result
  }

  "All methods of accessing data produce the same result" in {
    fail("write me, or make me implicit in the next test")
  }

  "Performance test for different file-access methods" in {
    import planet7.timing.Timer._

    // TODO - CAS - 08/08/2014 - Timing runs should automatically be collated.
    val collator = new TimingCollator(3)
    for (i <- 1 to 20) {
      collator {
        val csv = Csv(testData("large_dataset.csv"))
        //      .renameAndRestructure("first_name" -> "First Name", "last_name", "fee paid")
        //      .remap("last_name" -> (_.toUpperCase))

        export(csv)
      }
    }

    collator.total.average must be < 200.0
  }

//  "An empty Csv2 behaves itself" in {
//    assert(Csv2("").rows=== Nil)
//    assert(Csv2("Some,Header,Columns").rows === Nil)
//  }
//
//  "Merge Csv2s, so that we can gather similar data from multiple sources" in {
//    val left = Csv2( """
//                      |ID,Name,Value
//                      |A,B,C
//                    """.stripMargin)
//
//    val right = Csv2( """
//                       |ID,Value,Name
//                       |D,F,E
//                     """.stripMargin).restructure("ID", "Name", "Value") // Put columns into the same order
//
//    assert(Csv2(left, right) === Csv2( """
//                                       |ID,Name,Value
//                                       |A,B,C
//                                       |D,E,F
//                                     """.stripMargin))
//  }
//
//  "We can add columns to Csv2s" in {
//    val twoColumnsOfData = """
//                             |ID,Name
//                             |A,B
//                           """.stripMargin
//
//    val threeColumnsOfData = """
//                               |ID,Value,Name
//                               |A,Some default value,B
//                             """.stripMargin
//
//    def transform(data: String) =
//      Csv2(twoColumnsOfData)
//        .restructure("ID", "Value", "Name")
//        .remap("Value" -> (_ => "Some default value"))
//
//    assert(transform(twoColumnsOfData) === Csv2(threeColumnsOfData))
//
//  }
//
//  "The default String representation of Csv2 is the entire contents, formatted as a Csv2" in {
//    assert(Csv2(
//      List("foo", "bar"),
//      List(
//        List("one", "two"),
//        List("uno", "dos"),
//        List("ichi", "ni"),
//        List("eins", "zwei"))).toString ===
//      """foo,bar
//        |one,two
//        |uno,dos
//        |ichi,ni
//        |eins,zwei
//        |""".stripMargin)
//  }
//
//  "For debugging and REPL development, Csv2 provides a truncated String" in {
//    assert(Csv2(
//      List("foo", "bar"),
//      List(
//        List("one", "two"),
//        List("uno", "dos"),
//        List("ichi", "ni"),
//        List("eins", "zwei"))).toTruncString ===
//      """foo,bar
//        |one,two
//        |uno,dos
//        |ichi,ni
//        |...""".stripMargin)
//  }
}