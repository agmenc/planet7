package planet7.tabular

import planet7.tabular.StoreInRowValidations._

object FailFastValidations {
  def rowNotTruncated(header: Row)(row: Row): Row =
    if (StoreInRowValidations.rowNotTruncated(header)(row).isInvalid) throw new TruncatedDataRowException(header, row)
    else row
}

object StoreInRowValidations {
  def rowNotTruncated(header: Row)(row: Row): Row =
    if (row.data.length < header.data.length) row.copy(validationFailures = rowTruncatedMessage(header, row) +: row.validationFailures)
    else row

  def describe(header: Row, row: Row): String = (header.data zip row.data) map { case (heading, element) => s"$heading: $element" } mkString "\n"

  def rowTruncatedMessage(header: Row, row: Row) =
    s"""
       |The header contains ${header.data.length} elements, but the data row only contains ${row.data.length}:
       |${describe(header, row)}
       |""".stripMargin
}

class TruncatedDataRowException(header: Row, row: Row) extends RuntimeException {
  override def getMessage = rowTruncatedMessage(header, row)
}