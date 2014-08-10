package planet7.tabular

/**
 * A Csv is a Traversable input source and a set of transformations
 * A Csv provides a Traversable of ???
 *
 * All materialisation of the datasource is done OUTSIDE of the Csv.
 *
 * TODO - CAS - 07/08/2014 - A Y-shaped pipeline (spits out two CSVs)
 * TODO - CAS - 07/08/2014 - Aggregator 1 - combine multiple columns
 * TODO - CAS - 07/08/2014 - Aggregator 2 - combine multiple rows - provide a predicate for row grouping/inclusion/exclusion
 */
case class Csv(source: TabularDataSource, columnStructureTx: Row => Row = identity, headerRenameTx: Row => Row = identity) {
  def header: Row = headerRenameTx(columnStructureTx(source.header))

  def rows: Traversable[Row] = source.rows.map(columnStructureTx)

  def columnStructure(columns: (String, String)*): Csv = {
    val headerRow: Row = header
    val lookup: Array[Int] = columns.map{case (sourceCol, targetCol) => headerRow.data.indexOf(sourceCol)}(collection.breakOut)
    val newColumnStructureTx: Row => Row = row => Row(lookup.map(row.data))

    val columnRenames = Map(columns:_*)
    val newHeaderRenamer: Row => Row = row => Row(row.data.map(columnRenames))
    Csv(source, columnStructureTx andThen newColumnStructureTx, headerRenameTx andThen newHeaderRenamer)
  }

  // TODO - CAS - 08/08/2014 - Use withFilter on the Traversable[Row], as filter materialises the list when it filters it
}

//object Csv {
//  def apply[A](x: A)(implicit f: A => RelationalDataSource): Csv = Csv(f(x))
//}