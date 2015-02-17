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

/**
* Simple parser with common defaults:
*   - Quoted strings are preserved, even with embedded delimiters, e.g.:
*       foo, "one, monkey, two", bar ===> Array("foo", "one, monkey, two", "bar")
*   - spaces around elements are ignored (unless quoted)
*       foo    ,  bar ,   "   monkey one  ", baz ===> Array("foo", "bar", "   monkey one  ", "baz")
*/
class RegexTwoPassParser(val delimiter: Char) extends Parser {
  override val delim = escapeIfRegex(delimiter)

  private val quotes = s"""\""""
  private val wideDelim = s""" *$delim *"""
  private val token = "§‡".filterNot(_ == delimiter)

  private def escapeIfRegex(delimCandidate: Char): String = delimCandidate match {
    case '|' => "\\|"
    case x => s"$x"
  }

  override def read(line: String): Row = {
    def splitEvenItems(y: (String, Int)): Array[String] =
      if (y._2 % 2 == 0) splitDelimitedString(y._1)
      else Array(y._1)

    def splitDelimitedString(string: String): Array[String] = {
      val replaced: String = string.replaceAll(s"^$wideDelim|$wideDelim$$|$token", "")
      if (replaced.isEmpty) Array[String]()
      else replaced.split(wideDelim, -1)
    }

    Row((s"$token${line.trim}$token" split(quotes, -1) zipWithIndex) flatMap splitEvenItems)
  }

  override def write(row: Row) = row.data map quoteDelims mkString s"$delimiter"

  private def quoteDelims(elem: String): String = if (elem.contains(delim)) s""""$elem"""" else elem
}

object RegexTwoPassParser {
  def apply(delimiter: Char): RegexTwoPassParser = new RegexTwoPassParser(delimiter)
}