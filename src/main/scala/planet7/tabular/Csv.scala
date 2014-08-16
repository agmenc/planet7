package planet7.tabular

/**
 * A Csv is a Traversable input source and a set of transformations
 * A Csv provides a Traversable of ???
 *
 * All materialisation of the datasource is done OUTSIDE of the Csv.
 *
 *
 * // TODO - CAS - 12/08/2014 - A CSV should just be an iterator of Rows, with some extra gubbins around it
 *  - Iterators give the nicest once-only approach
 *  - Csv should be closeable - or returned iterator could close streams on next() == null
 *  - Kevin has given me dispensation to use mutability inside my iterator
 *
 *
 * TODO - CAS - 07/08/2014 - A Y-shaped pipeline (spits out two CSVs)
 * TODO - CAS - 07/08/2014 - Aggregator 1 - combine multiple columns
 * TODO - CAS - 07/08/2014 - Aggregator 2 - combine multiple rows - provide a predicate for row grouping/inclusion/exclusion
 */
case class Csv(source: TabularDataSource, columnStructureTx: Row => Row = identity, headerRenameTx: Row => Row = identity) {
  def header: Row = headerRenameTx(columnStructureTx(source.header))

  def rows: Iterator[Row] = source.rows.map(columnStructureTx)

  def columnStructure(columns: (String, String)*): Csv = Csv(
    source,
    columnStructureTx andThen nextColumnStructureTx(columns: _*),
    headerRenameTx andThen nextHeaderRenameTx(columns: _*)
  )

  private[tabular] def nextColumnStructureTx(columns: (String, String)*): (Row) => Row = {
    val headerRow: Row = header
    val lookup: Array[Int] = columns.map { case (sourceCol, targetCol) => headerRow.data.indexOf(sourceCol) }(collection.breakOut)

    // TODO - CAS - 15/08/2014 - If row has fewer elements than lookup, i.e. it is invalid, this fn throws ArrayIndexOutOfBoundsException
    row => Row(lookup.map(row.data))
  }

  private[tabular] def nextHeaderRenameTx(columns: (String, String)*) = (row: Row) => Row(row.data.map(Map(columns: _*)))

  // TODO - CAS - 08/08/2014 - Use withFilter on the Iterator[Row], as filter materialises the list when it filters it
}

//object Csv {
//  def apply[A](x: A)(implicit f: A => RelationalDataSource): Csv = Csv(f(x))
//}