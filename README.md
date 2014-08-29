planet7
=======

Fast, functional ETL and rec tool for Scala users:
* Load and merge CSVs.
* Rename, re-order and filter columns.
* Diff two CSVs

### Usage

**build.sbt:**

```scala
libraryDependencies += "com.github.agmenc" %% "planet7" % 0.0.8
```

**Extract and remodel a CSV:**

```scala
// CSV file with header: First name,Surname,Company,Company account,Postcode,Pet names
val someFile = asFile("before.csv")

// Retain only three of the original columns, in a different order, renaming "Postcode" to "Zip code", and adding "Fee owing"
val reshapedCsv = Csv(someFile)
  .columnStructure("Surname", "First name", "Postcode" -> "Zip code", "Fee owing")
  .withMappings(
    "Zip code" -> postcodeLookupTable,  // Map the old postcodes to zip codes, using a lookup table (Map)
    "Surname" -> (_.toUpperCase),       // Make all surnames upper case
    "Fee owing" -> (_ => "0.00")        // Add a default value for "Fee owing" of 0.00
  )

// Now convert the data to your data model, or export to a feed, or reconcile against another source, etc.
// reshapedCsv.rows map ( ... )
```

