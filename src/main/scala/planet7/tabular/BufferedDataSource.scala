package planet7.tabular

import java.io.{BufferedReader, Reader}

import scala.collection.AbstractTraversable

class BufferedDataSource(source: Reader) extends TabularDataSource {
  val lines = new LineReader(new BufferedReader(source))
  val header = lines.next()

  override def rows = lines
  override def close() = source.close()
}

class LineReader(lines: BufferedReader) extends Iterator[Row] {
  private var line = nextNonEmptyLine

  override def hasNext = line != null

  override def next() = {
    val oldLine = line
    line = nextNonEmptyLine
    if (line == null) lines.close()
    toRow(oldLine)
  }

  private def nextNonEmptyLine = {
    var l = lines.readLine()
    while (l != null && l.isEmpty) {
      l = lines.readLine()
    }
    l
  }
}