package planet7.tabular

case class LineParser(delimiter: Char) {
  private val delim = s"$delimiter"
  private[tabular] def read(line: String): Row = Row(line.split(delim, -1))
  private[tabular] def write(row: Row): String = row.data.mkString(delim)
}

object Parser {
  def default: LineParser = new LineParser(',')
  def apply(delimiter: Char): LineParser = new LineParser(delimiter)
}
