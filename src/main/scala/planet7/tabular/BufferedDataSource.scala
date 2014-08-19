package planet7.tabular

import java.io.{BufferedReader, Reader}

class BufferedDataSource(source: Reader) extends TabularDataSource {
  val lines = new LineReader(new BufferedReader(source))
  val header = if (lines.hasNext) lines.next() else throw new NoDataInSourceException(source.toString)

  override def rows = lines
  override def close() = source.close()
}

class LineReader(lines: BufferedReader) extends Iterator[Row] {
  private var line = nextNonEmptyLine

  override def hasNext = line != null && !line.trim.isEmpty

  override def next() = {
    val oldLine = line
    line = nextNonEmptyLine
    if (line == null) lines.close()
    toRow(oldLine)
  }

  private def nextNonEmptyLine = {
    var nnel = lines.readLine()
    while (nnel != null && nnel.trim.isEmpty) { nnel = lines.readLine() }
    nnel
  }
}