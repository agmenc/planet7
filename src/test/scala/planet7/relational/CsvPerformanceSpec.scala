package planet7.relational

import org.scalatest.WordSpec
import planet7.timing._
import TestData._

class CsvPerformanceSpec extends WordSpec {
  "We can read a large dataset in X seconds" in {
    val timer = new Timer(3)
    import timer._

    for (n <- 1 to 20) {
      val x = t"fragments" {
        val fileStream = t"load" {asInputStream("large_dataset.csv")}
        val csv = t"create" {Csv(fileStream)}
        val renamed = t"rename" {csv.rename("first_name" -> "First Name")}
        val restructured = t"restructure" {renamed.restructure("First Name", "last_name", "fee paid")}
        val remapped = t"remap" {restructured.remap("last_name" -> (_.toUpperCase))}

        remapped.toString
      }

      val y = t"oneShot" {
        val one = Csv(asInputStream("large_dataset.csv"))
          .renameAndRestructure("first_name" -> "First Name", "last_name", "fee paid")
          .remap("last_name" -> (_.toUpperCase))

        one.toString
      }
    }

    println(s"fragments: ${timer.fragments.average}")
    println(s"oneShot: ${timer.oneShot.average}")

    assert(timer.oneShot.average < 350)

    // Average timings for oneShot across 20 iterations, dropping the first three to allow for JIT compilations
    // List:      530 ms
    // Seq:       410 ms
    // Iterator:  160 ms
    // Vector: ...




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