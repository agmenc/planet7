package planet7.tabular

/**
 * Csv encapsulates a tabular data structure, as found in a CSV file or spreadsheet. It allows
 * client code to:
 *  - read CSV data from a tabular data-source (String, File, InputStream, or anything else)
 *  - Change the column structure (add, remove, rename and reorder columns)
 *  - Map data values according to a function (e.g. a lookup, a data-conversion, a default value)
 *  - Apply fail-fast or reporting-only validations to each row
 *  - Export the results
 *
 * This class may be useful for:
 *  - ETL of externally-generated data
 *  - Pre-formatting data before Diffing it during a reconciliation. @see Diff
 *
 * Csv is designed to be lazy. The datasource is traversed ONCE, when accessed via the Csv.iterator
 * method. All other operations are declarative (restructuring, data transformations, validations,
 * etc). Thus, the time cost of traversing the data and applying these functions is only paid
 * once, when the client code materialises the Csv.
 *
 * TODO - CAS - 07/08/2014 - A Y-shaped pipeline (spits out two CSVs)
 * TODO - CAS - 07/08/2014 - Aggregator 1 - merge columns using a Row => Row (e.g. (firstName, surname) -> s"$firstName $surname")
 * TODO - CAS - 07/08/2014 - Aggregator 2 - merge rows - provide a predicate for row grouping/inclusion/exclusion
 */
case class Csv(header: Row, private val dataRows: Iterator[Row]) extends Iterable[Row] {

  def columnStructure(columns: (String, String)*): Csv = {
    val columnXformer = columnXformerFor(columns: _*)
    val headerRenamer = headerRenamerFor(columns: _*)
    this.copy(header = headerRenamer(columnXformer(header)), dataRows = dataRows.map(columnXformer))
  }

  def columnStructure(headerRestructurer: Array[String] => Array[String]): Csv = columnStructure(headerRestructurer(header.data) map (s => s -> s) :_*)

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
    val desiredMappings: Map[Int, (String) => String] = mappings.map { case (column, mapper) => Row.indexOf(header, column) -> mapper }(collection.breakOut)

    desiredMappings foreach {
      case (index, mapper) => row.data(index) = mapper(row.data(index))
    }

    Row(row.data)
  }

  def assertAndReport(validationsToAdd: (Row => Row => Row)*): Csv =  this.copy(
    dataRows = dataRows map validateAgainst(validationsToAdd map Validations.catchingUnexpectedExceptions))

  def assertAndAbort(validationsToAdd: (Row => Row => Row)*): Csv = this.copy(
    dataRows = dataRows map validateAgainst(validationsToAdd map Validations.failFast))

  def filter(predicates: (String, String => Boolean)*): Csv = this.copy(dataRows = dataRows.withFilter(nextRowFilter(predicates:_*)))

  private[tabular] def nextRowFilter(predicates: (String, String => Boolean)*): Row => Boolean = row => predicates.forall {
    case (columnName, predicate) => predicate(row.data(header.data.indexOf(columnName)))
  }
  
  private def validateAgainst(validations: Seq[Row => Row => Row]): Row => Row = {
    val actualValidations: Seq[Row => Row] = validations.map(_(header))
    (row: Row) => actualValidations.foldLeft(row)((r,v) => v(r))
  }

  override def iterator = dataRows
}

object Csv {
  def apply[A](x: A)(implicit f: A => TabularDataSource): Csv = {
    val dataSource = f(x)
    Csv(dataSource.header, dataSource.rows)
  }

  // TODO - CAS - 07/12/14 - Check all CSVs have the same header
  def apply(csvs: Csv*): Csv = Csv(csvs.head.header, csvs.foldLeft(Iterator[Row]())((i: Iterator[Row], c: Csv) => i ++ c.dataRows))
}