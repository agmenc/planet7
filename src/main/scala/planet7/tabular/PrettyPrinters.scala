package planet7.tabular

trait PrettyPrinters {
  // Neatly export the first 5 lines of a CSV, fixed-width, with column names.
  def top5(csv: Csv): String = new CsvPrinter().top5(csv)

  // Show the differences between the rows in an under-and-over format, so that they are easier to see
  def showDiffs(left: Row, right: Row): String = new RowPrinter().showDiffs(left, right)
}