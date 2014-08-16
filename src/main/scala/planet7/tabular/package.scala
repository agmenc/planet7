package planet7

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

package object tabular {
  implicit def fromString(s: String): TabularDataSource = new BufferedDataSource(new StringReader(s))
  implicit def fromFile(f: File): TabularDataSource = new BufferedDataSource(new FileReader(f))
  implicit def fromInputStream(is: InputStream): TabularDataSource = new BufferedDataSource(new InputStreamReader(is))

  // 210 ms. About the same speed as the LineReader approach for a several-MB file, but much uglier
  def experimentalFromMemoryMappedFile(f: File): TabularDataSource = {
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
  def experimentalFromWholeFile(f: File): TabularDataSource = fromInputStream(new ByteArrayInputStream(Files.readAllBytes(Paths.get(f.toURI))))

  // 384 ms. Should only be used on small files, whatever that means
  def experimentalFromScanner(f: File): TabularDataSource = new ScannerDataSource(f)

  def toRow(line: String) = Row(line.split(","))

  def export(csv: Csv): String = csv.header.toString + "\n" + csv.rows.mkString("\n")

  /**
   * Allows us to specify a single String for a column name which is not changing, instead of a Tuple.
   */
  implicit def toColumnStructure(s: String): (String, String) = s -> s
}