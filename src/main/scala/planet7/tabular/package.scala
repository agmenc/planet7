package planet7

import java.util.Comparator

package object tabular extends DataSourceLoaders {
  def export(csv: Csv, parser: Parser = Parsers.basic): String = parser.write(csv.header) + "\n" + csv.iterator.map(parser.write).mkString("\n")

  def sort(csv: Csv, fieldComps: (String, Comparator[String])*): Csv = sort(csv, new RowDiffer(csv.header, fieldComps:_*))

  def sort(csv: Csv, differ: RowDiffer): Csv = Csv(csv.header, csv.iterator.toSeq.sorted(differ.ordering).iterator)

  def by[K: Ordering](f: String => K): Ordering[String] = new Ordering[String] {
    override def compare(x: String, y: String) = implicitly[Ordering[K]].compare(f(x), f(y))
  }

  def ignore(columnNames: String*) = (headerColumns: Array[String]) => headerColumns filter (col => !columnNames.contains(col))

  /** Used in Csv.columnStructure(). Puts a single columnName String into a before/after Tuple. */
  implicit def toColumnStructure(s: String): (String, String) = s -> s

  /** Used in sort(). Converts a single String into a Tuple with a basic Comparator[String]. */
  implicit def toStringCompare(s: String): (String, Comparator[String]) = s -> new Comparator[String] {
    override def compare(o1: String, o2: String) = o1.compareTo(o2)
  }

  /** Used in Cvs.assertAndAbort() and Csv.assertAndReport(), to build simple validation checks */
  implicit def toValidation(columnAssertion: (String, String => Boolean)): Row => Row => Row =
    (header: Row) => (row: Row) => {
      val (columnName, validationFunction) = columnAssertion
      if (validationFunction(row.data(Row.indexOf(header, columnName)))) row
      else row.copy(validationFailures = s"Validation failed for column: '$columnName'\n${Validations.describe(header, row)}" +: row.validationFailures)
    }
}