planet7
=======

Fast, functional ETL and rec tool for Scala users:
* Load and merge CSVs.
* Rename, re-order and filter columns.
* Diff two CSVs
* Optionally, use external CSV parsing libraries to load your data

### Supported Scala versions

* [scala 2.10.4](http://www.scala-lang.org)
* [scala 2.11.1](http://www.scala-lang.org)

### Usage

**build.sbt:**

```scala
libraryDependencies += "com.github.agmenc" %% "planet7" % 0.0.8
```


**Extract and remodel a CSV:**

```scala
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
```


**Export a CSV:**

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
import LargeDataSet._

val csv = Csv(CSVReader.open(asFile(largeDataFile)))

csv.header must equal(expectedHeader)
csv.rows.next() must be (expectedFirstRow)
```


**Diff/reconciliation:**

Convert two CSVs to a canonical format and Diff them, aggregating and formatting the output for easy reconciliation:

```scala
import planet7.Diff
import planet7.relational.CompanyAccountsData._

val before = Csv(asFile("before.csv"))
  .columnStructure("First name", "Surname", "Company", "Company account" -> "Company ID", "Postcode")
  .withMappings(
    "Postcode" -> postcodeLookupTable,
    "Company" -> (_.toUpperCase)
  )

val after = Csv(asFile("after_with_diffs.csv"))
  .columnStructure("First name", "Surname", "Company", "Company ID", "Postcode")

val diffs: Seq[(Row, Row)] = Diff(before.rows, after.rows, RowDiffer(3))

// The resulting diffs are yours to play with. Let's group them: missing rows, added rows, or just plain different rows
val summary = diffs.groupBy {
  case (row, EmptyRow) => "Missing"
  case (EmptyRow, row) => "Added"
  case (row1, row2) => "Diffs"
}

// We can Diff rows which have changed. We zip the header information with each row, so that we know the names of the fields which changed.
val fieldDifferences = summary("Diffs") map {
  case (leftRow, rightRow) => Diff(before.header.data zip leftRow.data, after.header.data zip rightRow.data, FieldDiffer)
}

// Let's print the name of the field which changed, and the before and after values
val readableDiffs = fieldDifferences map (FieldDiffer.prettyPrint(_).mkString(", "))
printSummary(summary, readableDiffs)
assert(readableDiffs === List(
  "Postcode: 43205 -> 432666, Company: ENIM SIT AMET INCORPORATED -> ENIM SIT AMET LIMITED",
  "Postcode: 22656 -> 22756"
))
```


**Laziness:**

All structure and filtering operations are lazy. Aside from the header, the datasource is only read when the Csv is exported, or you iterate through the Csv.rows iterator. 


**More examples:**

See [CsvSpec.scala](https://github.com/agmenc/planet7/blob/master/src/test/scala/planet7/tabular/CsvSpec.scala) for a full range of working examples 

