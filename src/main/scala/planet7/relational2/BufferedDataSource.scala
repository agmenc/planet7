package planet7.relational2

import java.io.{BufferedReader, Reader}

class BufferedDataSource(source: Reader) extends RelationalDataSource {
  val lines: BufferedReader = new BufferedReader(source)
  val header = toRow(lines.readLine())
  override def rows = new LineReader(lines)
}