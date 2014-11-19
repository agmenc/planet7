package planet7.tabular

case class LineParser(delimiter: Char) {
  private[tabular] def read(aLine: String): Row = ???
  private[tabular] def write(row: Row): String = ???
}

object Parser {
  def default: LineParser = new LineParser(',')
  def apply(delimiter: Char): LineParser = new LineParser(delimiter)
}
