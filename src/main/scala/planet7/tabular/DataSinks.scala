package planet7.tabular

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

trait DataSinks {
  def write(csv: Csv, path: String) = {
    val writer = new FileWriter(path)
    def writeRow(row: Row) = writer.write(s"${row.toString}\n".toCharArray)
    writeRow(csv.header)
    csv.iterator.foreach(writeRow)
    writer.flush()
    writer.close()
  }
}