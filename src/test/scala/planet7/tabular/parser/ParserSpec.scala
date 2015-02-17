package planet7.tabular.parser

import org.scalatest.{MustMatchers, WordSpec}
import planet7.tabular.LargeDataSet._
import planet7.tabular._
import planet7.timing.Timer

import scala.io.Source

class ParserSpec extends WordSpec with MustMatchers {
  "Adjacent commas create empty data fields" in {
    Parsers.basic.read("0,,,,") must equal (Row(Array("0", "", "", "", "")))
  }

  "We can specify the delimiter when reading a CSV" in {
    val input = "Index\tName\tValue\nD\tE\tF\nG\tH\tI"

    val result = """Index,Name,Value
                   |D,E,F
                   |G,H,I""".stripMargin

    val csv = Csv(fromString(input, new DefaultParser('\t')))

    export(csv) mustEqual result
  }

  "We can specify the delimiter when writing a CSV" in {
    val input = """Index,Name,Value
                  |D,E,F
                  |G,H,I""".stripMargin

    val result = """Index-Name-Value
                   |D-E-F
                   |G-H-I""".stripMargin

    val csv = Csv(fromString(input))

    export(csv, new DefaultParser('-')) mustEqual result
  }

  "We can read data with quoted delimiters" in {
    new RegexTwoPassParser('-').read("""Index-Name-"Value-of-thing"""") mustEqual Row(Array("Index","Name","Value-of-thing"))
    new RegexTwoPassParser('_').read(""""Date_of_birth"_Weight__""_Sex_""") mustEqual Row(Array("Date_of_birth","Weight", "", "", "Sex", ""))
  }

  /**
      RegexTwoPassParser        36.41 ms (avg. of 17 readings)
           DefaultParser         7.12 ms (avg. of 17 readings)
  */
  "Each Parser type performs tolerably" in {
    val timer = new Timer(3)
    import planet7.timing._
    import timer._

    val data = Source fromFile TestDataFile(largeDataFile) getLines() take 1000 toList

    for {
      parser <- Seq(
        new DefaultParser(','),
        new RegexTwoPassParser(','))
      i <- 1 to 20
    } t"${parser.getClass.getSimpleName}" {
      if (i == 1) println(parser.getClass.getSimpleName)
      data foreach parser.read
    }

    println(timer)

    timer.DefaultParser.average must be < 8.0
    timer.RegexTwoPassParser.average must be < 60.0
  }

  "We keep spaces inside quotes, and trim them outside" in {
    val input = """"Index "- " Name " -some-values-  "Value-of-thing  "-normal-again"""
    new RegexTwoPassParser('-').read(input) mustEqual Row(Array("Index "," Name ","some", "values", "Value-of-thing  ", "normal", "again"))
  }

  "Empty quotes become empty elements" in {
    new RegexTwoPassParser('-').read("""foo- ""  -baa""") mustEqual Row(Array("foo","","baa"))
  }

  "Quotes are discarded unless necessary" in {
    new RegexTwoPassParser(',').write(Row(Array("Index","Name","Value-of-thing", "Id"))) mustEqual """Index,Name,Value-of-thing,Id"""
    new RegexTwoPassParser('-').write(Row(Array("Index","Name","Value-of-thing", "Id"))) mustEqual """Index-Name-"Value-of-thing"-Id"""
  }

  "The RegexTwoPassParser trims all data elements" in {
    val spaceyInput = """     First Name , Surname, Age
                        | Sue, Smith, 24
                        |Bob    ,Smith  , 24
                        | Fred , "Black   "    , 127
                        | Jeremiah Jehosephat     ,     Jones,36     """.stripMargin

    val trimmedOutput = """First Name,Surname,Age
                   |Sue,Smith,24
                   |Bob,Smith,24
                   |Fred,Black   ,127
                   |Jeremiah Jehosephat,Jones,36""".stripMargin

    val parser = RegexTwoPassParser(',')
    val parsed = spaceyInput.split("\n").map(parser.read).map(parser.write).mkString("\n")

    parsed mustEqual trimmedOutput
  }

  "We can use a single § as a delimiter" in {
    val dataStructure = Row(Array("one", "two", "three"))

    Seq('§', '±', '~', '`'/*, '|'*/) foreach {delim =>
      val flatFile = s"one${delim}two${delim}three"
      val parser = RegexTwoPassParser(delim)
      parser.read(flatFile) mustEqual dataStructure
      parser.write(dataStructure) mustEqual flatFile
    }
  }
}