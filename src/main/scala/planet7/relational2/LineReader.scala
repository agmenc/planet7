package planet7.relational2

import java.io.BufferedReader

import scala.collection.AbstractTraversable

class LineReader(lines: BufferedReader) extends AbstractTraversable[Row] {
  override def foreach[U](f: (Row) => U) = {
    var line = lines.readLine()
    while (line != null) {
      if (line.nonEmpty) f(toRow(line))
      line = lines.readLine()
    }
  }
}