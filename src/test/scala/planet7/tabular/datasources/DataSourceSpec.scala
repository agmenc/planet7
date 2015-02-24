package planet7.tabular.datasources

import org.scalatest.mock.MockitoSugar
import org.scalatest.{MustMatchers, WordSpec}

class DataSourceSpec extends WordSpec with MustMatchers with MockitoSugar {
  "We can concatenate datasources" in {
    import planet7.tabular._

    val one = fromString( """a,b,c
                            |1,2,3
                          """.stripMargin)

    val two = fromString( """a,b,c
                            |4,5,6
                          """.stripMargin)

    val three = fromString( """a,b,c
                              |7,8,9
                            """.stripMargin)

    combine(one, two, three).rows.mkString("\n") mustEqual """1,2,3
                                                             |4,5,6
                                                             |7,8,9""".stripMargin
  }
}