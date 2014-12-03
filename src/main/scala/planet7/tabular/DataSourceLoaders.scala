package planet7.tabular

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}

trait DataSourceLoaders {
  implicit def fromString(s: String): TabularDataSource = fromString(s, Parsers.basic)
  implicit def fromFile(f: File): TabularDataSource = fromFile(f, Parsers.basic)

  // TODO - CAS - 03/12/14 - Find out why this compiles ad runs in the IDE but not in SBT
//  implicit def fromFile(f: scala.reflect.io.File): TabularDataSource = fromFile(f.jfile, Parsers.basic)

  implicit def fromInputStream(is: InputStream): TabularDataSource = fromInputStream(is, Parsers.basic)
  implicit def fromIterable(it: Iterable[String]): TabularDataSource = fromIterable(it, Parsers.basic)

  def fromString(s: String, parser: Parser): TabularDataSource = new BufferedDataSource(new StringReader(s), parser)
  def fromFile(f: File, parser: Parser): TabularDataSource = new BufferedDataSource(new FileReader(f), parser)
  def fromInputStream(is: InputStream, parser: Parser): TabularDataSource = new BufferedDataSource(new InputStreamReader(is), parser)

  implicit def fromIterable(it: Iterable[String], parser: Parser): TabularDataSource = new TabularDataSource {
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
    new BufferedDataSource(new CharArrayReader(cb.array()), Parsers.basic)
  }

  // 255 ms. Should only be used on small files, whatever that means
  def experimentalFromWholeFile(f: File): TabularDataSource = fromInputStream(new ByteArrayInputStream(Files.readAllBytes(Paths.get(f.toURI))))

  // 384 ms. Should only be used on small files, whatever that means
  def experimentalFromScanner(f: File): TabularDataSource = new ScannerDataSource(f, Parsers.basic)
}