package planet7.tabular

trait PrettyPrinters {
  // Print the first 5 lines of a CSV neatly, fixed-width, with column names
  def print(csv: Csv): String = new CsvPrinter().print(csv)

  // Show the differences between the rows in an under-and-over format, so that they are easier to see
  def showDiffs(left: Row, right: Row): String = new RowPrinter().showDiffs(left, right)
}