package planet7

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.util.Comparator

package object tabular {
  implicit def fromString(s: String): TabularDataSource = new BufferedDataSource(new StringReader(s))
  implicit def fromFile(f: File): TabularDataSource = new BufferedDataSource(new FileReader(f))
  implicit def fromInputStream(is: InputStream): TabularDataSource = new BufferedDataSource(new InputStreamReader(is))

  implicit def fromIterable(it: Iterable[String]): TabularDataSource = new TabularDataSource {
    private val lines = it.iterator
    override val header = toRow(lines.next())
    override def rows = lines map toRow
    override def close() = Unit
  }

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

  def sort(csv: Csv, fieldComps: (String, Comparator[String])*): Csv = SortAggregator.sort(csv, fieldComps:_*)

  def by[K: Ordering](f: String => K): Ordering[String] = new Ordering[String] {
    override def compare(x: String, y: String) = implicitly[Ordering[K]].compare(f(x), f(y))
  }

  /**
   * Allows us to specify a single String for a column name which is not changing, instead of a Tuple.
   */
  implicit def toColumnStructure(s: String): (String, String) = s -> s

  /**
   * Allows us to specify a single String for a column which is to be sorted alphanumerically, instead of a Tuple.
   */
  implicit def toOrderable(s: String): (String, String => Any) = s -> (str => str)
}