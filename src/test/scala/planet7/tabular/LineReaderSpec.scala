package planet7.tabular

import java.io.{BufferedReader, IOException, StringReader}

import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.{MustMatchers, WordSpec}

class LineReaderSpec extends WordSpec with MustMatchers with MockitoSugar {
  implicit def buffy(str: String) = new BufferedReader(new StringReader(str))

  val whitespace = "   \n   \t   \r   "
  val lines = s"""
      |one
      |two
      |
      |
      |three
      |
      |$whitespace
      |
    """.stripMargin

  "hasNext is false if there are no more non-empty lines to read" in {
    val lr = new LineReader(lines, Parser.default)

    lr.hasNext must be (true)
    lr.next().toString must be ("one")
    lr.next().toString must be ("two")
    lr.next().toString must be ("three")
    lr.hasNext must be (false)
  }

  "When the last line is empty, it is ignored by the iterator" in {
    val lr = new LineReader(lines, Parser.default)

    lr.next().toString must be ("one")
    lr.next().toString must be ("two")
    lr.next().toString must be ("three")

    an [IOException] should be thrownBy lr.next()
  }

  "When the iterator is exhausted, the data source is closed" in {
    val reader = mock[BufferedReader]
    when(reader.readLine()) thenReturn "One line only" thenReturn null

    val lr = new LineReader(reader, Parser.default)
    lr.next().toString must be ("One line only")

    verify(reader).close()
  }
}