package planet7.tabular.recipes

import java.io.File

import org.scalatest.{MustMatchers, WordSpec}

class ValidateCsvDataRowByRow extends WordSpec with MustMatchers {
  import planet7.tabular._

  val blacklistedCompanies = Seq("IT617FKFKI68991671283461524", "GB46OXEF88480659916871", "ES3619333624584565411377")
  def isValid(companyId: String) = !blacklistedCompanies.contains(companyId)

  "Fail fast if choosing to abort on validation failures" in {
    val companyData = new File("src/test/resources/planet7/tabular/after/new_company_format.csv")
    
    val csv = Csv(companyData).assertAndAbort( "Company ID" -> isValid _ )

    a [ValidationFailedException] should be thrownBy export(csv)
  }

  "Report all errors if choosing to report on validation failures" in {
    val companyData = new File("src/test/resources/planet7/tabular/after/new_company_format.csv")

    val csv = Csv(companyData).assertAndReport( "Company ID" -> isValid _ )

    // TODO - CAS - 05/01/15 - Demonstrate better how Row is used to store the failure messages
    val failureMessages: PartialFunction[Row, String] = { case row: Row if row.isInvalid => row.validationFailures.head }
    val allFailureMessages = csv.iterator.collect(failureMessages).mkString("\n")
    grep("Company ID:", allFailureMessages) must equal ("""Company ID: ES3619333624584565411377
                                                         |Company ID: GB46OXEF88480659916871
                                                         |Company ID: IT617FKFKI68991671283461524""".stripMargin)
  }

  def grep(pattern: String, text: String): String = {
    val regex = pattern.r
    text.split("\n", -1).filter(line => regex.findFirstIn(line).isDefined).mkString("\n")
  }
}