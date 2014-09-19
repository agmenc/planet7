package planet7

import java.util.Comparator

package object tabular extends DataSourceLoaders {
  def toRow(line: String) = Row(line.split(","))

  def export(csv: Csv): String = csv.header.toString + "\n" + csv.rows.mkString("\n")

  def sort(csv: Csv, fieldComps: (String, Comparator[String])*): Csv = sort(csv, new RowDiffer(csv.header, fieldComps:_*))

  def sort(csv: Csv, differ: RowDiffer): Csv = Csv(csv.header, csv.rows.toSeq.sorted(differ.ordering).iterator)

  def by[K: Ordering](f: String => K): Ordering[String] = new Ordering[String] {
    override def compare(x: String, y: String) = implicitly[Ordering[K]].compare(f(x), f(y))
  }

  /** Used in Csv.columnStructure(). Puts a single columnName String into a before/after Tuple. */
  implicit def toColumnStructure(s: String): (String, String) = s -> s

  /** Used in sort(). Converts a single String into a Tuple with a basic Comparator[String]. */
  implicit def toStringCompare(s: String): (String, Comparator[String]) = s -> new Comparator[String] {
    override def compare(o1: String, o2: String) = o1.compareTo(o2)
  }
}