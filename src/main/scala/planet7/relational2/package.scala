package planet7

import java.io._

package object relational2 {
  implicit def fromString(s: String): RelationalDataSource = new BufferedDataSource(new StringReader(s))
  implicit def fromFile(f: File): RelationalDataSource = new BufferedDataSource(new FileReader(f))
  implicit def fromInputStream(is: InputStream): RelationalDataSource = new BufferedDataSource(new InputStreamReader(is))

  def toRow(line: String) = Row(line.split(","))
  def export(csv: Csv): String = csv.source.header.toString + "\n" + csv.source.rows.mkString("\n")
}