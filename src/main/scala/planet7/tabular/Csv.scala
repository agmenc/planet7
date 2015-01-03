package planet7.tabular

/**
 * Csv encapsulates a tabular data structure, as found in a CSV file or spreadsheet. This class is a thin wrapper around an
 * Iterator[Row] and a header Row. It allows client code to:
 *  - read CSV data from a tabular data-source (String, File, InputStream, or anything else)
 *  - Change the column structure (add, remove, rename and reorder columns)
 *  - Map data values according to a function (e.g. a lookup, a data-conversion, a default value)
 *  - Export the results
 *
 * This class may be useful for:
 *  - ETL of externally-generated data
 *  - Pre-formatting data before Diffing it during a reconciliation. @see Diff
 *
 * Csv is designed to be lazy. All materialisation of the datasource is done OUTSIDE of the Csv, i.e. as late as possible. Thus,
 * there should be no appreciable time cost to constructing a Csv, merging Csvs, restructuring columns, and so forth. The time
 * cost is only paid once, when the client code exports or otherwise materialises the Csv.
 *
 * TODO - CAS - 07/08/2014 - A Y-shaped pipeline (spits out two CSVs)
 * TODO - CAS - 07/08/2014 - Aggregator 1 - combine multiple columns
 * TODO - CAS - 07/08/2014 - Aggregator 2 - combine multiple rows - provide a predicate for row grouping/inclusion/exclusion
 */
case class Csv(header: Row, private val dataRows: Iterator[Row], validations: Seq[Row => Row => Row] = Csv.defaultValidations) extends Iterable[Row] {

  def columnStructure(columns: (String, String)*): Csv = {
    val columnXformer = columnXformerFor(columns: _*)
    val headerRenamer = headerRenamerFor(columns: _*)
    this.copy(header = headerRenamer(columnXformer(header)), dataRows = dataRows.map(columnXformer))
  }

  def columnStructure(restructurer: Array[String] => Array[String]): Csv = columnStructure(restructurer(header.data) map (s => s -> s) :_*)

  private[tabular] def columnXformerFor(columns: (String, String)*): (Row) => Row = {
    val desiredColumnIndices: Array[Int] = columns.map { case (sourceCol, targetCol) => header.data.indexOf(sourceCol) }(collection.breakOut)

    row => Row(desiredColumnIndices filter (_ < row.data.length) map {
      case newColumn if newColumn == -1 => ""
      case validIndex => row.data(validIndex)
    })
  }

  private[tabular] def headerRenamerFor(columns: (String, String)*) = (row: Row) => Row(columns.map {
    case (before, after) => after
  }(collection.breakOut))

  def withMappings(mappings: (String, (String) => String)*): Csv = this.copy(dataRows = dataRows.map(valuesXformerFor(mappings: _*)))

  private[tabular] def valuesXformerFor(mappings: (String, (String) => String)*): Row => Row = (row: Row) => {
    def indexOf(column: String): Int = header.data.indexOf(column) match {
      case notFound if notFound < 0 => throw new ColumnDoesNotExistException(column, header)
      case ok => ok
    }

    val desiredMappings: Map[Int, (String) => String] = mappings.map { case (column, mapper) => indexOf(column) -> mapper }(collection.breakOut)

    desiredMappings foreach {
      case (index, mapper) => row.data(index) = mapper(row.data(index))
    }

    Row(row.data)
  }

  def tolerantReader() = asserting()

  def asserting(validations: (Row => Row => Row)*): Csv = this.copy(validations = validations)

  def filter(predicates: (String, String => Boolean)*): Csv = this.copy(dataRows = dataRows.withFilter(nextRowFilter(predicates:_*)))

  private[tabular] def nextRowFilter(predicates: (String, String => Boolean)*): Row => Boolean = row => predicates.forall {
    case (columnName, predicate) => predicate(row.data(header.data.indexOf(columnName)))
  }

  override def iterator = {
    val actualValidations: Seq[Row => Row] = validations.map(_(header))
    def blowIfBad(row: Row): Row = actualValidations.foldLeft(row)((r,v) => v(r))
    dataRows map blowIfBad
  }
}

object Csv {
  def apply[A](x: A)(implicit f: A => TabularDataSource): Csv = {
    val dataSource = f(x)
    Csv(dataSource.header, dataSource.rows)
  }

  // TODO - CAS - 07/12/14 - Check all CSVs have the same header
  // TODO - CAS - 31/12/14 - Should we also fold in the validations?
  def apply(csvs: Csv*): Csv = Csv(csvs.head.header, csvs.foldLeft(Iterator[Row]())((i: Iterator[Row], c: Csv) => i ++ c.dataRows))

  def defaultValidations: Seq[Row => Row => Row] = Seq(FailFastValidations.rowNotTruncated)
}

class ColumnDoesNotExistException(columnName: String, header: Row) extends RuntimeException {
  override def getMessage = s"Cannot find column '$columnName' in header with columns:\n${header.data.mkString("\n")}\n"
}