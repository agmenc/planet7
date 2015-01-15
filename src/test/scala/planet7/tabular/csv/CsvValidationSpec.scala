package planet7.tabular.csv

import org.scalatest.{MustMatchers, WordSpec}
import planet7.tabular._

class CsvValidationSpec extends WordSpec with MustMatchers {
  val brokenData = """
                     |val1,val2,val3
                     |1,2
                     |3,X,5""".stripMargin

  "Strict (normal) mode: applies default validations and throws exception when they break" in {
    a [ValidationFailedException] should be thrownBy export(Csv(brokenData))
  }

  "Tolerant mode: does NOT throw an exception when when data is invalid" in {
    val csv = Csv(brokenData).clearDefaultValidations()

    noException should be thrownBy export(csv)
  }

  "Fails fast if we try to use a bad column name in a data transformation" in {
    val input = """
                  |val1,val2,val3
                  |0,""".stripMargin

    val csv = Csv(input).withMappings("val840" -> (_.toUpperCase))

    a [ColumnDoesNotExistException] should be thrownBy export(csv)
  }

  "Validation failures are reported when using assertAndReport()" in {
    val csv = Csv(brokenData)
      .clearDefaultValidations()
      .assertAndReport(Validations.rowNotTruncated)

    csv.iterator.next().validationFailures.head must include ("The header contains 3 elements, but the data row only contains 2")
  }

  "Validation failures abort processing when using assertAndAbort()" in {
    val csv = Csv(brokenData).assertAndAbort(Validations.rowNotTruncated)

    a [ValidationFailedException] should be thrownBy export(csv)
  }

  def throwsNumberFormatException(dataValue: String): Boolean = dataValue.toInt > 0

  "Unexpected exceptions are caught and not rethrown when using assertAndReport()" in {
    val csv = Csv(brokenData)
      .clearDefaultValidations()
      .assertAndReport("val2" -> throwsNumberFormatException _)

    noException should be thrownBy export(csv)
  }

  "Unexpected exceptions are reported when using assertAndReport()" in {
    val csv = Csv(brokenData)
      .clearDefaultValidations()
      .assertAndReport("val2" -> throwsNumberFormatException _)

    val rows = csv.iterator
    rows.next()
    rows.next().validationFailures.head must include ("For input string: \"X\"")
  }

  "Unexpected exceptions abort processing when using assertAndAbort()" in {
    val csv = Csv(brokenData)
      .clearDefaultValidations()
      .assertAndAbort("val2" -> throwsNumberFormatException _)

    a [NumberFormatException] should be thrownBy export(csv)
  }

  def alwaysSucceeds(text: String): Boolean = true
  def alwaysFails(text: String): Boolean = false

  "We can mix aborting and reporting validations" in {
    val csv = Csv(brokenData)
      .clearDefaultValidations()
      .assertAndAbort("val1" -> alwaysSucceeds _)
      .assertAndReport("val1" -> alwaysFails _)

    noException should be thrownBy export(csv)
  }
}