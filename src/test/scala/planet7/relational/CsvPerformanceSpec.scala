package planet7.relational

import org.scalatest.WordSpec
import planet7.timing._
import TestData._

class CsvPerformanceSpec extends WordSpec {
  "We can read a large dataset in under 350 milliseconds" in {
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

    println(timer)

    assert(timer.oneShot.average < 350)

    // Average timings for oneShot across 20 iterations, dropping the first three to allow for JIT compilations
    // List:      530 ms
    // Seq:       410 ms
    // Iterator:  250 ms
    // Vector: ...
  }
}