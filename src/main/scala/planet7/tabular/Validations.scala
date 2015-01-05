package planet7.tabular

object Validations {
  def failFast(validation: Row => Row => Row): Row => Row => Row =
    (header: Row) => (row: Row) => {
      val resultRow = validation(header)(row)
      if (resultRow.isInvalid) throw new ValidationFailedException(resultRow.validationFailures.head)
      else resultRow
    }

  def catchingUnexpectedExceptions(validation: Row => Row => Row): Row => Row => Row =
    (header: Row) => (row: Row) =>
      try {validation(header)(row)}
      catch {
        case e: Exception => addFailure(row, s"${e.getMessage}\n${describe(header, row)}")
      }

  def describe(header: Row, row: Row): String = (header.data zip row.data) map { case (heading, element) => s"$heading: $element"} mkString "\n"

  def addFailure(row: Row, failureMsg: String): Row = row.copy(validationFailures = failureMsg +: row.validationFailures)

  def rowNotTruncated(header: Row)(row: Row): Row = {
    def rowTruncatedMessage(header: Row, row: Row) = s"""
       |The header contains ${header.data.length} elements, but the data row only contains ${row.data.length}:
       |${describe(header, row)}
       |""".stripMargin

    if (row.data.length < header.data.length) addFailure(row, rowTruncatedMessage(header, row)) else row
  }
}

class ValidationFailedException(msg: String) extends RuntimeException(msg)
