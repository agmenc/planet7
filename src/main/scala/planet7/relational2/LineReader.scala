package planet7.relational2

import java.io.BufferedReader

import scala.collection.AbstractTraversable

class LineReader(lines: BufferedReader) extends AbstractTraversable[Row] {
  override def foreach[U](f: (Row) => U) = {
    // 256 ms
    // Stream.continually(lines.readLine()).takeWhile(_ != null).withFilter(_.nonEmpty).foreach(line => f(toRow(line)))

    // 196 ms
    var line = lines.readLine()
    while (line != null) {
      if (line.nonEmpty) f(toRow(line))
      line = lines.readLine()
    }
  }
}