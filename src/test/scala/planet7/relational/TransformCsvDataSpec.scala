package planet7.relational

import org.scalatest.WordSpec

class TransformCsvDataSpec extends WordSpec {
  val input = """ID,Name,Value
                |A,B,C
                |D,E,F
                |G,H,I
                |J,K,L
                |M,N,O""".stripMargin

  val expectedOutput = """ID,Name,Value
                         |A,B,C
                         |G,H,I
                         |M,N,O
                         |""".stripMargin
  
  "We can filter rows according to a column-based predicate" in {
    val transformedCsv = Csv(input).filter(RowPredicates.and(
      "ID" -> {v => v != "D"},
      "ID" -> {v => v != "J"}
    ))

    assert(transformedCsv.toString === expectedOutput)
  }
}