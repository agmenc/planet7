package planet7.tabular.csv

import org.scalatest.{MustMatchers, WordSpec}
import planet7.tabular._

class ParserSpec extends  WordSpec with MustMatchers {
  "We can change the delimiter when reading a CSV" in {
    val input = "Index\tName\tValue\nD\tE\tF\nG\tH\tI"

    val result = """Index,Name,Value
                   |D,E,F
                   |G,H,I""".stripMargin

    val csv = Csv(fromString(input, Parser('\t')))

    export(csv) mustEqual result
  }

  "We can change the delimiter when writing a CSV" in {
    val input = """Index,Name,Value
                  |D,E,F
                  |G,H,I""".stripMargin

    val result = """Index-Name-Value
                   |D-E-F
                   |G-H-I""".stripMargin

    val csv = Csv(fromString(input))

    export(csv, Parser('-')) mustEqual result
  }

  "We can read data with quoted delimiters" in {
    Parser('-').read("""Index-Name-"Value-of-thing"""") mustEqual Row(Array("Index","Name","Value-of-thing"))
    Parser('_').read(""""Date_of_birth"_Weight__""_Sex_""") mustEqual Row(Array("Date_of_birth","Weight", "", "", "Sex", ""))
  }

  "We keep spaces inside quotes, and trim them outside" in {
    Parser('-').read(""""Index "- " Name " -  "Value-of-thing  " """) mustEqual Row(Array("Index "," Name ","Value-of-thing  "))
  }

  "Empty quotes become empty elements" in {
    Parser('-').read("""foo- ""  -baa""") mustEqual Row(Array("foo","","baa"))
  }

  "Quotes are discarded unless necessary" in {
    Parser(',').write(Row(Array("Index","Name","Value-of-thing", "Id"))) mustEqual """Index,Name,Value-of-thing,Id"""
    Parser('-').write(Row(Array("Index","Name","Value-of-thing", "Id"))) mustEqual """Index-Name-"Value-of-thing"-Id"""
  }

  "We understand how String.split works (sigh)" in {
    """Index-Name-"Value-of-thing"""".split("\"", -1) mustEqual Array("Index-Name-", "Value-of-thing", "")
    """"Date_of_birth"_Weight""".split("\"", -1) mustEqual Array("", "Date_of_birth", "_Weight")
    """"Date_of_birth"_Weight__""_Sex_""".split("\"", -1) mustEqual Array("", "Date_of_birth", "_Weight__", "", "_Sex_")
    "_Sex_".split("_", -1) mustEqual Array("", "Sex", "")
  }

  "We understand how to get a left/right list" in {
    val memoised: Stream[Boolean] = Stream from 1 map (x => x % 2 > 0)
    val tagged = List("a", "b", "c") zip memoised
    println(s"memoised: ${tagged}")
  }
}
