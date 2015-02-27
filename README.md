planet7
=======

Fast ETL and reconciliation tool for Scala users:
* Load and merge CSVs.
* Rename, re-order and filter columns.
* Sort and validate rows
* Diff two CSVs, or any lists of comparable elements
* Use external CSV parsing libraries such as CSVReader

### Usage

**Supported Scala versions:**

* [scala 2.10](http://www.scala-lang.org)
* [scala 2.11](http://www.scala-lang.org)

**build.sbt:**

```scala
libraryDependencies += "com.github.agmenc" %% "planet7" % "0.1.13"
```


**Basic ETL:**

In this example, we convert from a two-column input format:

| Name | Id |
|------|:--:|
| Bob  |  A |
| Dave |  C |

to a three-column output format:

| Id | Value | Name |
|:--:|-------|------|
|  1 | X     | bob  |
|  3 | X     | dave |


We also manipulate the data:
* Change the names to lower case
* Default "Value" to "X"
* Map the characters "A" and "C" to the numerals "1" and "3".


```scala
"We can map data and specify default values by column" in {
  val twoColumns = Csv("""
                         |Name,ID
                         |BOB,A
                         |DAVE,C
                       """.stripMargin)

  val threeColumns = Csv("""
                           |ID,Value,Name
                           |1,X,bob
                           |3,X,dave
                         """.stripMargin)

  def alphaToNum(alpha: String): String = alpha match {
    case "A" => "1"
    case _ => "3"
  }

  val result = twoColumns
    .columnStructure("ID", "Value", "Name")
    .withMappings(
      "Value" -> (_ => "X"),      // Default value (for the empty column we added)
      "ID" -> alphaToNum,         // Mapping function
      "Name" -> (_.toLowerCase)   // Mapping function
    )

  result.rows.toList must equal(threeColumns.rows.toList)
}
```

### All CSV Features

```scala
  "All available CSV-manipulation features" in {
    import planet7.tabular._

    val csv = Csv(new File(inputPath))
      .assertAndAbort(                      // Fail-fast validations
        Validations.rowNotTruncated)        // Let's check early, before we select columns
      .columnStructure(
        "Company",                          // Keep any column we name; remove the rest
        "Company account" -> "Company ID",  // Rename "Company account" to "Company ID"
        "First name",
        "Surname",
        "Postcode" -> "Zip code")
      .withMappings(
        "Zip code" -> postcodeLookupTable,  // Specify a (String) => String to change data
        "Surname" -> (_.toUpperCase))
      .assertAndReport(                     // Reported validations are appended to the row
        "Zip code" -> validZipCode _)       // Report any invalid zip codes
      .columnStructure(ignore("Zip code"))  // Drop column, now we've validated against it

    write(sort(csv), outputPath)            // Sort the output and write to disk

    Diff(Csv(new File(outputPath)), Csv(new File(modelAnswerPath)), NaiveRowDiffer) mustBe empty
  }
  // ```

### More Examples

These examples are taken from [CsvSpec.scala](https://github.com/agmenc/planet7/blob/master/src/test/scala/planet7/tabular/CsvSpec.scala). They are not exhaustive, so see [CsvSpec.scala](https://github.com/agmenc/planet7/blob/master/src/test/scala/planet7/tabular/CsvSpec.scala) for a full range of working examples.


**Extract and remodel a CSV:**

```scala
"Extract a CSV, remodel it, and convert the data" in {
  import planet7.tabular.CompanyAccountsData._

  // CSV file with header: First name,Surname,Company,Company account,Postcode,Pet names
  val someFile = asFile("before.csv")

  // Retain only three of the original columns, in a different order, renaming
  // "Postcode" to "Zip code", and adding "Fee owing"
  val reshapedCsv = Csv(someFile)
    .columnStructure("Surname", "First name", "Postcode" -> "Zip code", "Fee owing")
    .withMappings(
      "Zip code" -> postcodeLookupTable,  // Map the old postcodes to zip codes, using a Map
      "Surname" -> (_.toUpperCase),       // Make all surnames upper case
      "Fee owing" -> (_ => "0.00")        // Add a default value for "Fee owing" of 0.00
    )

  // Now convert the data to your data model, or export to a feed, or reconcile against another source, etc.
  // reshapedCsv.rows map ( ... )
}
```


**Export a CSV:**

Materialises the data. Use this in tests or for small files, but consider iterating through Csv.rows and writing to a BufferedWriter or similar, to avoid sucking all your data into the heap at once.

```scala
val allYourData: String = export(reshapedCsv)
```


**Use an external CSV parser:**

Pimp external libraries into TabularDataSources:

```scala
implicit def fromCsvReader(reader: CSVReader): TabularDataSource = new TabularDataSource {
  override val header = reader.readNext() match {
    case Some(items) => Row(items.toArray)
    case None => throw new NoDataInSourceException(reader.toString)
  }

  override def rows = reader.iterator.map(items => Row(items.toArray))

  override def close() = reader.close()
}
```

Now work with Csv in the usual way:

```scala
"We can use external parsers such as (the incredibly slow) CSVReader" in {
  import LargeDataSet._

  val csv = Csv(CSVReader.open(asFile(largeDataFile)))

  csv.header must equal(expectedHeader)
  csv.rows.next() must be (expectedFirstRow)
}
```


**Diff/reconciliation:**

Convert two CSVs to a canonical format and Diff them, aggregating and formatting the output for easy reconciliation:

```scala
"We can Diff Csv instances and generate readable output" in {
  import planet7.Diff
  import planet7.tabular.CompanyAccountsData._

  val before = Csv(asFile("before.csv"))
    .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
    .withMappings(
      "Postcode" -> postcodeLookupTable,
      "Company" -> (_.toUpperCase)
    )

  val after = Csv(asFile("after_with_diffs.csv"))
    .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

  val diffs: Seq[(Row, Row)] = Diff(before, after, RowDiffer(before.header, "Company ID"))

  // The resulting diffs are yours to play with. Let's group them: missing rows, added rows, or just plain different rows.
  val summary = diffs.groupBy {
    case (row, EmptyRow) => "Missing"
    case (EmptyRow, row) => "Added"
    case (row1, row2) => "Diffs"
  }

  // We can Diff rows which have changed. We zip the header information with each row, so that we know the names of the fields which changed.
  val fieldDifferences = summary("Diffs") map {
    case (leftRow, rightRow) => NonSortingDiff(before.header.data zip leftRow.data, after.header.data zip rightRow.data, FieldDiffer)
  }

  // Let's print the name of the field which changed, and the before and after values
  val readableDiffs = fieldDifferences map (FieldDiffer.prettyPrint(_).mkString(", "))
  printSummary(summary, readableDiffs)
  assert(readableDiffs === List(
    "Postcode: 43205 -> 432666, Company: ENIM SIT AMET INCORPORATED -> ENIM SIT AMET LIMITED",
    "Postcode: 22656 -> 22756"
  ))
}
```


**Laziness:**

All structure and filtering operations are lazy. Aside from the header row, the datasource is only read when the Csv is exported, or you iterate through the Csv.rows iterator. 


**Timers:**

External parsers, large datasets or naive coding can slow down CSV transformations. Don't optmise prematurely; instead, time your operations and fix only those that need fixing:


```scala
"Users of the planet7 library can gauge the performance impact of external parsers such as CsvReader" in {
  import planet7.timing._

  val timer = new Timer(2)
  import timer._

  for (i <- 1 to 5) {
    t"overallTime" {
      val csvReader = t"shouldBeQuick" { CSVReader.open(asFile("before.csv")) }
      val csv = t"shouldAlsoBeQuick" { Csv(csvReader) }
      t"veryExpensive" { export(csv) }
    }
  }

  println(timer)
  timer.overallTime.average must be < 150.0
}
```


**More examples:**

See [CsvSpec.scala](https://github.com/agmenc/planet7/blob/master/src/test/scala/planet7/tabular/CsvSpec.scala) for a full range of working examples 

