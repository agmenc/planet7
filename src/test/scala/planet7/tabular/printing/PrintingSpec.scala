package planet7.tabular.printing

import org.scalatest.{MustMatchers, WordSpec}
import planet7.tabular._

class PrintingSpec extends WordSpec with MustMatchers {
  private val data = """
                       |ID,Owed amount,Name
                       |1,12.3,bob
                       |2,52.7,dave
                       |3,26.8,sue
                       |4,52.7,merideth
                       |5,52.5,fred
                       |6,84.5,frank
                       |7,38.4,jo
                       |8,14.6,nick
                     """.stripMargin

  "We can pretty-print the Csv for quick feedback in the REPL or while developing tests" in {
    val expected = """
                     |ID  Owed amount      Name
                     |--  -----------  --------
                     | 1         12.3       bob
                     | 2         52.7      dave
                     | 3         26.8       sue
                     | 4         52.7  merideth
                     | 5         52.5      fred
                     |""".stripMargin

    val csv = Csv(data)

    new CsvPrinter().top5(csv) mustEqual expected
  }
}