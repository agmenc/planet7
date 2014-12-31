package planet7.tabular

object Validations {
  // Example of a fail-fast validation: the exception brings processing to a halt
  def rowNotTruncated(header: Row)(row: Row): Row = if (row.data.length < header.data.length) throw new TruncatedDataRowException(header, row) else row
}

class TruncatedDataRowException(header: Row, row: Row) extends RuntimeException {
  def describeRow: String = (header.data zip row.data) map { case (heading, element) => s"$heading: $element" } mkString "\n"
  override def getMessage =
    s"""
       |The header contains ${header.data.length} elements, but the data row only contains ${row.data.length}:
       |$describeRow
       |""".stripMargin
}