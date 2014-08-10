package planet7.tabular

import java.io.{BufferedReader, Reader}

class BufferedDataSource(source: Reader) extends TabularDataSource {
  val lines: BufferedReader = new BufferedReader(source)

  val header = {
    val line = lines.readLine()
    if (line != null) toRow(line) else throw new EmptyFileException
  }

  override def rows = new LineReader(lines)
}