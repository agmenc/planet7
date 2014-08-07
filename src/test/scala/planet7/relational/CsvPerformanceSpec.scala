package planet7.relational

import org.scalatest.WordSpec
import planet7.timing.Timer._
import TestData._

class CsvPerformanceSpec extends WordSpec {
  "We can read a large dataset in X seconds" in {
    val fragCollator = new TimingCollator(1)
    val oneShotCollator = new TimingCollator(1)

    for (n <- 1 to 20) {
      {
        val fragmentedTimer = fragCollator.time
        import fragmentedTimer._
        val fileStream = t"load" {readFileToInputStream("large_dataset.csv")}
        val csv = t"create" {Csv(fileStream)}
        val renamed = t"rename" {csv.rename("first_name" -> "First Name")}
        val restructured = t"restructure" {renamed.restructure("First Name", "last_name", "fee paid")}
        val remapped = t"remap" {restructured.remap("last_name" -> (_.toUpperCase))}
        println(s"Fragmented timer.total: ${fragmentedTimer.total}")
      }

      {
        val oneShotTimer = oneShotCollator.time
        import oneShotTimer._
        val x = t"one-shot" {
          Csv(readFile("large_dataset.csv"))
            .renameAndRestructure("first_name" -> "First Name", "last_name", "fee paid")
            .remap("last_name" -> (_.toUpperCase))
        }
        println(s"oneShotTimer.total: ${oneShotTimer.total}")
      }
    }

    println(s"collator.total: ${fragCollator.total}")
    println(s"collator.total.average: ${fragCollator.total.average}")
    println("\n")
    println(s"oneShot collator.total: ${oneShotCollator.total}")
    println(s"oneShot collator.total.average: ${oneShotCollator.total.average}")
    println("\n")
    println(s"collator.load.average: ${fragCollator.load.average}")
    println(s"collator.create.average: ${fragCollator.create.average}")
    println(s"collator.rename.average: ${fragCollator.rename.average}")
    println(s"collator.restructure.average: ${fragCollator.restructure.average}")
    println(s"collator.remap.average: ${fragCollator.remap.average}")
    println("\n")

    // Drop the first
    // Average the next ten
    // record in a comment the average time for each collection type, e.g.:
    // List: 530 ms
    // Seq:  410 ms
    // Vector: ...


    // Test the all-in-one method
    // Test the diff
    // Create some benchmarks and assert that we stay within them




//      .rename("Company account" -> "Company ID")
//      .restructure("First name", "Surname", "Company", "Company ID", "Postcode")
//      .remap(
//        "Postcode" -> postcodeLookupTable,
//        "Company" -> (_.toUpperCase)
//      )

//    val after = Csv(readFile("after_with_diffs.csv"))
//      .restructure("First name", "Surname", "Company", "Company ID", "Postcode")
//
//    val diffs: List[(Row, Row)] = Diff(before.rows, after.rows, RowDiffer("Company ID"))
//
//    val summary = diffs.groupBy {
//      case (row, EmptyRow) => "Missing"
//      case (EmptyRow, row) => "Added"
//      case (row1, row2) => "Diffs"
//    }
//
//    val readableDiffs = summary("Diffs") map (d => Diff(d._1.values, d._2.values, FieldDiffer)) map (prettyPrint(_).mkString(", "))
//
//    assert(readableDiffs === List(
//      "Postcode: 43205 -> 432666, Company: ENIM SIT AMET INCORPORATED -> ENIM SIT AMET LIMITED",
//      "Postcode: 22656 -> 22756"
//    ))
  }

  "We can gauge the performance impact of external parsers" in {

  }
}