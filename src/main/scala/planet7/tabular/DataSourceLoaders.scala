package planet7.tabular

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}

trait DataSourceLoaders {
  implicit def fromString(s: String): TabularDataSource = fromString(s, Parser.default)
  implicit def fromFile(f: File): TabularDataSource = fromFile(f, Parser.default)
  implicit def fromInputStream(is: InputStream): TabularDataSource = fromInputStream(is, Parser.default)
  implicit def fromIterable(it: Iterable[String]): TabularDataSource = fromIterable(it, Parser.default)

  def fromString(s: String, parser: LineParser): TabularDataSource = new BufferedDataSource(new StringReader(s), parser)
  def fromFile(f: File, parser: LineParser): TabularDataSource = new BufferedDataSource(new FileReader(f), parser)
  def fromInputStream(is: InputStream, parser: LineParser): TabularDataSource = new BufferedDataSource(new InputStreamReader(is), parser)

  implicit def fromIterable(it: Iterable[String], parser: LineParser): TabularDataSource = new TabularDataSource {
    private val lines = it.iterator
    override val header = parser.read(lines.next())
    override def rows = lines map parser.read
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
    new BufferedDataSource(new CharArrayReader(cb.array()), Parser.default)
  }

  // 255 ms. Should only be used on small files, whatever that means
  def experimentalFromWholeFile(f: File): TabularDataSource = fromInputStream(new ByteArrayInputStream(Files.readAllBytes(Paths.get(f.toURI))))

  // 384 ms. Should only be used on small files, whatever that means
  def experimentalFromScanner(f: File): TabularDataSource = new ScannerDataSource(f, Parser.default)
}