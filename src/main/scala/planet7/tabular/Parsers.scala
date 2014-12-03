package planet7.tabular

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex.{MatchIterator, Match}

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

  private def wideDelim = s""" *$delimiter *"""
  private val quotes = s"""$wideDelim{1}\"|\"$wideDelim{1}|^\"|\"$$"""

  override def read(line: String): Row = {
    def splitEvenItems(y: (String, Int)): Array[String] =
      if (y._2 % 2 == 0)
        if (y._1.isEmpty) Array[String]()
        else y._1.split(wideDelim, -1)
      else Array(y._1)

    Row((line split(quotes, -1) zipWithIndex).flatMap(splitEvenItems))
  }

  override def write(row: Row) = row.data map quoteDelims mkString delim

  private def quoteDelims(elem: String): String = if (elem.contains(delimiter)) s""""$elem"""" else elem
}