package planet7.tabular.csv

import com.github.tototoshi.csv.CSVReader
import org.scalatest.{MustMatchers, WordSpec}
import planet7.tabular._
import planet7.tabular.datasources.CsvReaderDataSource

class CsvPerformanceSpec extends WordSpec with MustMatchers {
  import planet7.tabular.csv.CsvConsistencySpec._

  /**
   * Data is sourced from Mockaroo. To regenerate:
   * curl http://www.mockaroo.com/7aa9b980/download?count=1000 > "My Saved Schema.csv"
   *
   * Typical results, given:
   *  (1) Run as a single test, via the IDE (so it isn't competing with other tests for CPU)
   *  (2) -XX:MaxPermSize=256m -Xms1024m -Xmx2048m -Xss2m
   *  (3) The CPU isn't overloaded with other tasks (e.g. bloated browser tabs, other IDE projects, ...)
   *

           exp. scanner       273.94 ms (avg. of 17 readings)
      stringInputStream       259.76 ms (avg. of 17 readings)
                 string       230.18 ms (avg. of 17 readings)
         exp. wholeFile       166.00 ms (avg. of 17 readings)
  exp. memoryMappedFile       120.65 ms (avg. of 17 readings)
                   file        96.24 ms (avg. of 17 readings)
        fileInputStream        93.47 ms (avg. of 17 readings)

   */
  "Performance test for different file-access methods" in {
    import planet7.tabular.LargeDataSet._
    import planet7.timing._

    def processLargeDataset(datasource: TabularDataSource) = export(
      Csv(datasource)
        .columnStructure("first_name" -> "First Name", "last_name", "fee paid")
        .withMappings("last_name" -> (_.toUpperCase))
    )

    val timer = new Timer(3)
    import timer._

    for {
      (label, loadMethod) <- possibleLoadMethods(largeDataFile)
      i <- 1 to 20
    } t"$label" {
      if (i == 1) println(label)
      processLargeDataset(loadMethod())
    }

    println(timer)
    timer.file.average must be < 180.0
  }

  // 143 seconds to load 25000 rows, i.e. 1,000 times slower than just reading the file into Csv Rows. Hells bells.
  "Users of the planet7 library can gauge the performance impact of external parsers such as CsvReader" in {
    import CsvReaderDataSource._
    import planet7.timing._

    val timer = new Timer(2)
    import timer._

    for (i <- 1 to 5) {
      t"overallTime" {
        val csvReader = t"shouldBeQuick" { CSVReader.open(TestDataFile("before.csv")) }
        val csv = t"shouldAlsoBeQuick" { Csv(csvReader) }
        t"veryExpensive" { export(csv) }
      }
    }

    println(timer)
    timer.overallTime.average must be < 150.0
  }
}