package planet7.tabular

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

trait DataSinkImplicits {
  def write(csv: Csv, path: String, parser: Parser = Parsers.basic) = {
    val writer = new FileWriter(path)
    def writeRow(row: Row) = writer.write(s"${parser.write(row)}\n".toCharArray)
    writeRow(csv.header)
    csv.iterator.foreach(writeRow)
    writer.flush()
    writer.close()
  }
}