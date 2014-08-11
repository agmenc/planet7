package planet7.tabular

import java.io.{BufferedReader, Reader}

import scala.collection.AbstractTraversable

class BufferedDataSource(source: Reader) extends TabularDataSource {
  val lines: BufferedReader = new BufferedReader(source)

  val header = {
    val line = lines.readLine()
    if (line != null) toRow(line) else throw new EmptyFileException
  }

  override def rows(columnStructureTx: Row => Row) = new LineReader(lines, columnStructureTx)
}

class LineReader(lines: BufferedReader, columnStructureTx: Row => Row) extends AbstractTraversable[Row] {
  override def foreach[U](f: (Row) => U) = {
    // 256 ms
    // Stream.continually(lines.readLine()).takeWhile(_ != null).withFilter(_.nonEmpty).foreach(line => f(toRow(line)))

    // 196 ms
    var line = lines.readLine()
    while (line != null) {
      if (line.nonEmpty) f(columnStructureTx(toRow(line)))
      line = lines.readLine()
    }
  }
}