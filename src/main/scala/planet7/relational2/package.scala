package planet7

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

package object relational2 {
  implicit def fromString(s: String): RelationalDataSource = new BufferedDataSource(new StringReader(s))
  implicit def fromFile(f: File): RelationalDataSource = new BufferedDataSource(new FileReader(f))
  implicit def fromInputStream(is: InputStream): RelationalDataSource = new BufferedDataSource(new InputStreamReader(is))

  // About the same speed as the LineReader approach for a 3.5 MB file, but much uglier
  def experimentalFromMemoryMappedFile(f: File): RelationalDataSource = {
    val rf = new RandomAccessFile(f, "r")
    val decoder = Charset.defaultCharset().newDecoder()
    val ch = rf.getChannel
    val buffer = ByteBuffer.allocate(ch.size.asInstanceOf[Int])
    ch.read(buffer)
    buffer.flip()
    val cb = decoder.decode(buffer)
    new BufferedDataSource(new CharArrayReader(cb.array()))
  }

  // 255 ms. Should only be used on small files, whatever that means
  def experimentalFromWholeFile(f: File): RelationalDataSource = fromInputStream(new ByteArrayInputStream(Files.readAllBytes(Paths.get(f.toURI))))

  def toRow(line: String) = Row(line.split(","))
  def export(csv: Csv): String = csv.source.header.toString + "\n" + csv.source.rows.mkString("\n")
}