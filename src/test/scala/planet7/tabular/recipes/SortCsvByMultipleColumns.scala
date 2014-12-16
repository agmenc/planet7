package planet7.tabular.recipes

import org.scalatest.{MustMatchers, WordSpec}

class SortCsvByMultipleColumns extends WordSpec with MustMatchers {
  "We can sort a Csv by column names, converting non-Strings to appropriate types" in {
    import planet7.tabular._

    val input = Csv( """First Name,Surname,Age
                       |Sue,Smith,24
                       |Jim,Jones,36
                       |Bob,Smith,24
                       |Fred,Black,127
                       |Jeremiah,Jones,36""".stripMargin)

    val expectedOutput = Csv( """First Name,Surname,Age
                                |Bob,Smith,24
                                |Sue,Smith,24
                                |Jeremiah,Jones,36
                                |Jim,Jones,36
                                |Fred,Black,127""".stripMargin)

    val result = sort(input,
      "Age" -> by(_.toInt),
      "Surname",
      "First Name" -> by(_.substring(0, 3))
    )

    export(result) must equal (export(expectedOutput))
  }
}