package planet7.tabular

trait Parser {
  def delim: String
  def read(line: String): Row = Row(line.split(delim, -1))
  def write(row: Row): String = row.data.mkString(delim)
}

object Parsers {
  def basic: Parser = new DefaultParser(',')
}

case class DefaultParser(delimiter: Char) extends Parser {
  override val delim = s"$delimiter"
}

case class RegexTwoPassParser(delimiter: Char) extends Parser {
  override val delim = s"$delimiter"

  private val quotes = s"""\""""
  private val wideDelim = s""" *$delimiter *"""

  override def read(line: String): Row = {
    def splitEvenItems(y: (String, Int)): Array[String] =
      if (y._2 % 2 == 0) splitDelimitedString(y._1)
      else Array(y._1)

    def splitDelimitedString(string: String): Array[String] = {
      val replaced: String = string.replaceAll(s"^$wideDelim|$wideDelim$$|§", "")
      if (replaced.isEmpty) Array[String]()
      else replaced.split(wideDelim, -1)
    }

    Row((s"§$line§" split(quotes, -1) zipWithIndex) flatMap splitEvenItems)
  }

  override def write(row: Row) = row.data map quoteDelims mkString delim

  private def quoteDelims(elem: String): String = if (elem.contains(delimiter)) s""""$elem"""" else elem
}
